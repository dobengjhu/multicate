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
#' @importFrom dplyr n
#' @importFrom data.table :=
#' @importFrom stats var qt
#' @importFrom rlang .data
#' @param object list. An object resulting from \link{estimate_cate}.
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
#' @example inst/examples/example-predict_cate.R
#' @export
predict.cate <- function(object,
                         newdata_tbl,
                         alpha = 0.05) {
  assertthat::assert_that(
    inherits(object, "cate"),
    msg = "use only with \"cate\" objects"
  )
  
  assertthat::assert_that(
    any(class(newdata_tbl) %in% c("tbl", "data.frame")),
    msg = "newdata_tbl` must be a tibble or data.frame."
  )

  assertthat::assert_that(
    object$aggregation_method == "studyindicator",
    msg = "Aggregation method must be 'studyindicator'."
  )

  assertthat::assert_that(
    is.numeric(alpha),
    msg = "`alpha` must be a numeric value between 0 and 1."
  )

  assertthat::assert_that(
    dplyr::between(alpha, 0, 1),
    msg = "`alpha` must be a numeric value between 0 and 1."
  )

  model <- object$model
  S <- rlang::sym(object$study_col)
  W <- rlang::sym(object$treatment_col)
  Y <- rlang::sym(object$outcome)
  K <- model %>%
    dplyr::distinct(!!S) %>%
    dplyr::pull()

  assertthat::assert_that(
    length(K) > 2,
    msg = "Insufficient degrees of freedom; original model must include at least 3 studies."
  )

  required_colnames <- model %>%
    dplyr::select(!!W, !!!rlang::syms(object$covariate_col)) %>%
    colnames()

  assertthat::assert_that(
    all(required_colnames %in% colnames(newdata_tbl)),
    msg = glue::glue("New data must include same treatment and covariate columns used \\
                     to fit original model")
  )

  original_colnames <- setdiff(colnames(newdata_tbl), as.character(S))

  newdata <- newdata_tbl %>%
    dplyr::slice(rep(1:n(), each = length(K))) %>%
    dplyr::mutate(!!S := rep(K, times = nrow(newdata_tbl)))

  newfeat <- newdata %>%
    {
      if (object$estimation_method == "causalforest") {
        dplyr::select(., !!S, dplyr::all_of(object$covariate_col))
      } else {
        dplyr::select(., !!W, !!S, dplyr::all_of(object$covariate_col))
      }
    } %>%
    fastDummies::dummy_cols(select_columns = as.character(S),
                            remove_selected_columns = TRUE)

  if (object$estimation_method == "causalforest" &
      object$aggregation_method == "studyindicator") {

    predict_causal_forest <- get("predict.causal_forest", envir = asNamespace("grf"))
    relevant_args <- object$extra_args[names(object$extra_args) %in% names(formals(predict_causal_forest))]
    newdata_pred <- do.call(predict_causal_forest,
                            c(list(object = object$estimation_object,
                                   newdata = newfeat,
                                   estimate.variance = TRUE),
                              relevant_args))

    newdata <- newdata %>%
      dplyr::mutate(tau_predicted = newdata_pred$predictions,
                    tau_var = newdata_pred$variance.estimates)
  } else if (object$estimation_method == "slearner" &
             object$aggregation_method == "studyindicator") {
    newfeat_counterfactual <- newfeat %>%
      dplyr::mutate(!!W := as.numeric(!!W == 0))

    predict_bart <- get("predict.bart", envir = asNamespace("dbarts"))
    relevant_args <- object$extra_args[names(object$extra_args) %in% names(formals(predict_bart))]
    newdata_pred <- do.call(predict_bart,
                            c(list(object = object$estimation_object,
                                   newdata = newfeat),
                              relevant_args))
    newdata_pred_counterfactual <- do.call(predict_bart,
                                           c(list(object = object$estimation_object,
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
    dplyr::summarise(mean_tau_predicted = mean(.data$tau_predicted),
                     var_within = mean(.data$tau_var),
                     var_between = var(.data$tau_predicted),
                     n_K = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var_tot = .data$var_within + .data$var_between,
                  sig = sqrt(.data$var_tot),
                  lower = .data$mean_tau_predicted - qt(1 - alpha/2, .data$n_K - 2) * .data$sig,
                  upper = .data$mean_tau_predicted + qt(1 - alpha/2, .data$n_K - 2) * .data$sig) %>%
    dplyr::select(dplyr::all_of(original_colnames),
                  tau_predicted = "mean_tau_predicted",
                  ci_lower = "lower",
                  ci_upper = "upper")

  newdata_tbl %>%
    dplyr::left_join(cis, by = original_colnames)
}
