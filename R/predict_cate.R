#' Predict Heterogeneous Treatment Effects Based on Estimated CATEs
#'
#' @param cate_obj list. An object resulting from \link{estimate_cate}.
#' @param newdata_tbl tbl. A tbl containing columns for treatment, outcome, study ID, and any
#'  additional covariates used to generate `cate_obj`. All study data must be included in single
#'  tbl. Note that only two treatments can be considered and treatment must be coded as 0/1
#'  (numeric).
#' @param alpha numeric. Significance level for confidence intervals. Defaults to 0.05 for 95% CIs.
#'
#' @return A list
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
