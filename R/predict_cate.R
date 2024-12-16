#' Predict Heterogeneous Treatment Effects Based on Estimated CATEs
#'
#' @description
#' `predict_cate` is used to produce a prediction of the conditional average treatment effect (CATE)
#' based on a model built on multiple studies.
#'
#' @details
#' The predict function uses estimated CATEs from multiple studies (objects resulting from
#' \link{estimate_cate}) to form prediction intervals for CATEs in a new, target setting (e.g., a
#' group of patients in an electronic health record system). This function leverages prediction
#' interval techniques from meta-analysis by estimating the within-study and between-study variance
#' of the CATEs for all covariate profiles (X) of interest in the target setting. The prediction
#' interval for the CATE for covariate profile X in the target setting is of the form:
#' \deqn{\hat{\tau}(\mathbf{X}^*) \pm \sqrt{var_{within} + var_{between}}}
#' Where \eqn{\hat{\tau}(\mathbf{X}^*)} is the inverse variance-weighted average of the
#' \eqn{\hat{\tau}(\mathbf{X}^*)} across studies, \eqn{t_{K-2}} is the t-statistic with K-2 degrees
#' of freedom, var_within is the average of the variances of each \eqn{\hat{\tau}(\mathbf{X}^*)},
#' and var_between is the variance of the set of \eqn{\hat{\tau}(\mathbf{X}^*)}. For more details,
#' see Brantner et al., (2024).
#'
#' The predict function can only be run on `cate` objects with variance estimates available. As
#' long as that criteria is met, any method used to estimate the CATEs across multiple studies can
#' then be used to create prediction intervals using this approach.
#'
#' The `newdata_tbl` can consist of any set of profiles for which the researcher would like to
#' create prediction intervals. The assumption with this approach is that the profiles in the target
#' setting can be found within the covariate distribution of at least one of the studies used to
#' estimate the CATE (i.e., positivity). CATE prediction intervals will be constructed separately
#' for each covariate profile, represented by a row in the newdata_tbl.
#'
#' @references
#' Brantner CL, Nguyen TQ, Parikh H, Zhao C, Hong H, Stuart EA. Precision mental health: Predicting
#' heterogeneous treatment effects for depression through data integration. *Arxiv*.
#'
#' @param cate_obj list. An object resulting from \link{estimate_cate}.
#' @param newdata_tbl tbl. A tbl containing columns for treatment, outcome, study ID, and any
#'  additional covariates used to generate `cate_obj`. All study data must be included in single
#'  tbl. Note that only two treatments can be considered and treatment must be coded as 0/1
#'  (numeric).
#' @param alpha numeric. Significance level for confidence intervals. Defaults to 0.05 for 95% CIs.
#'
#' @return A tbl including all columns and data from `newdata_tbl`, with additional columns
#' including a mean predicted CATE and associated 100(1 - \eqn{\alpha})% prediction interval.
#' @export
#'
#' @examples
predict.cate <- function(cate_obj,
                         newdata_tbl,
                         alpha = 0.05) {

  assertthat::assert_that(
    cate_obj$estimation_method %in% c("causalforest", "slearner"),
    msg = "Estimation method must be 'causalforest' or 'slearner'."
  )

  assertthat::assert_that(
    cate_obj$aggregation_method == "studyindicator",
    msg = "Aggregation method must be 'studyindicator'."
  )

  model <- cate_obj$model
  S <- rlang::sym(cate_obj$study_col)
  W <- rlang::sym(cate_obj$treatment_col)
  Y <- rlang::sym(cate_obj$outcome)
  covariates <- rlang::syms(cate_obj$covariate_col)

  required_colnames <- model %>%
    dplyr::select(!!W, !!!covariates) %>%
    colnames()

  assertthat::assert_that(
    all(required_colnames %in% colnames(newdata_tbl)),
    msg = glue::glue("New data must include same treatment, outcome, and covariate columns used \\
                     to fit original model")
  )

  original_colnames <- setdiff(colnames(newdata_tbl), as.character(S))

  K <- model %>%
    distinct(!!S) %>%
    nrow()

  newdata <- newdata_tbl %>%
    dplyr::slice(rep(1:n(), each = K)) %>%
    dplyr::mutate(!!S := rep(1:K, times = nrow(newdata_tbl)))

  newfeat <- newdata %>%
    {
      if (cate_obj$estimation_method == "causalforest") {
        dplyr::select(., !!S, dplyr::all_of(covariate_col))
      } else {
        dplyr::select(., !!W, !!S, dplyr::all_of(covariate_col))
      }
    } %>%
    fastDummies::dummy_cols(select_columns = as.character(S),
                            remove_selected_columns = TRUE)

  if (cate_obj$estimation_method == "causalforest" &
      cate_obj$aggregation_method == "studyindicator") {
    relevant_args <- cate_obj$extra_args[names(cate_obj$extra_args) %in% names(formals(grf:::predict.causal_forest))]
    newdata_pred <- do.call(grf:::predict.causal_forest,
                            c(list(object = cate_obj$estimation_object,
                                   newdata = newfeat,
                                   estimate.variance = TRUE),
                              relevant_args))

    newdata <- newdata %>%
      dplyr::mutate(tau_predicted = newdata_pred$predictions,
                    tau_var = newdata_pred$variance.estimates)
  } else if (cate_obj$estimation_method == "slearner" &
             cate_obj$aggregation_method == "studyindicator") {
    newfeat_counterfactual <- newfeat %>%
      dplyr::mutate(!!W := as.numeric(!!W == 0))

    relevant_args <- cate_obj$extra_args[names(cate_obj$extra_args) %in% names(formals(dbarts:::predict.bart))]
    newdata_pred <- do.call(dbarts:::predict.bart,
                            c(list(object = cate_obj$estimation_object,
                                   newdata = newfeat),
                              relevant_args))
    newdata_pred_counterfactual <- do.call(dbarts:::predict.bart,
                                           c(list(object = cate_obj$estimation_object,
                                                  newdata = newfeat_counterfactual),
                                             relevant_args))

    tau_list <- estimate_sbart_tau(newdata_pred,
                                   newdata_pred_counterfactual,
                                   newdata[[as.character(W)]])
    newdata <- newdata %>%
      dplyr::mutate(tau_predicted = tau_list$means_cate,
                    tau_var = tau_list$vars_cate)
  }

  cis <- newdata %>%
    dplyr::group_by(!!!rlang::syms(original_colnames)) %>%
    dplyr::summarise(mean_tau_predicted = mean(tau_predicted),
                     var_within = mean(tau_var),
                     var_between = var(tau_predicted),
                     n_K = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var_tot = var_within + var_between,
                  sd = sqrt(var_tot),
                  lower = mean_tau_predicted - qt(1 - alpha/2, n_K - 2) * sd,
                  upper = mean_tau_predicted + qt(1 - alpha/2, n_K - 2) * sd)

  newdata_tbl %>%
    left_join(cis, by = original_colnames)
}
