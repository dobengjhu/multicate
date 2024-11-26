#' Generate CATE Estimates Using Single-Study Approach with Study Indicator
#'
#' @param named_args list. A list of pre-specified argument values from \link{estimate_cate}.
#' @param ... list. Arguments to be passed to `estimation_method` and/or `aggregation_method`.
#'
#' @return A list with the following elements:
#'  - `tau_hat`: predicted conditional average treatment effect for each observation
#'  - `variance_estimates`: variance estimate for each tau_hat (not available when
#'    `estimation_method` set to "xlearner" or `aggregation_method` set to "ensembleforest)
#'  - `var_importance`: TBD
#'  - `fit_object`: object from \link[grf:causal_forest]{grf::causal_forest},
#'  \link[causalToolbox:X_RF]{causalToolbox::X_RF}, or \link[dbarts:bart]{dbarts::bart}, according
#'  to the `estimation_method` selected.
generate_tau <- function(named_args,
                         ...) {
  UseMethod("generate_tau")
}

#' @export
#' @rdname generate_tau
generate_tau.studyindicator_args <- function(named_args,
                                             ...) {
  tau_args <- prepare_cate_data(
    named_args,
    "studyindicator"
  )

  if (named_args$estimation_method == "causalforest") {
    extra_args <- list(...)
    relevant_args <- extra_args[names(extra_args) %in% names(formals(grf::causal_forest))]
    tau <- do.call(grf::causal_forest,
                   c(list(X = tau_args$feature_tbl,
                          W = tau_args$treatment_vec,
                          Y = tau_args$outcome_vec),
                     relevant_args))

    var_import <- tibble::tibble(
      variable = colnames(tau_args$feature_tbl),
      importance = as.numeric(grf::variable_importance(tau)),
    )

    return(list(tau_hat = as.numeric(tau$predictions),
                variance_estimates = predict(tau,
                                             estimate.variance = TRUE)$variance.estimates,
                var_importance = var_import,
                fit_object = tau))
  } else if (named_args$estimation_method == "xlearner") {
    extra_args <- list(...)
    relevant_args <- extra_args[names(extra_args) %in% names(formals(causalToolbox::X_RF))]
    xrf_fit <- do.call(causalToolbox::X_RF,
                       c(list(feat = tau_args$feature_tbl,
                              tr = tau_args$treatment_vec,
                              yobs = tau_args$outcome_vec),
                         relevant_args))

    return(list(tau_hat = causalToolbox::EstimateCate(xrf_fit,
                                                      tau_args$feature_tbl),
                variance_estimates = NULL,
                var_importance = NULL,
                fit_object = xrf_fit))
  } else if (named_args$estimation_method == "slearner") {
    extra_args <- list(...)
    relevant_args <- extra_args[names(extra_args) %in% names(formals(dbarts::bart))]
    relevant_args[["keeptrees"]] <- TRUE

    sbart_fit <- do.call(dbarts::bart,
                         c(list(x.train = tau_args$feature_tbl %>%
                                  dplyr::mutate(W = tau_args$treatment_vec),
                                y.train = tau_args$outcome_vec,
                                x.test = tau_args$feature_tbl %>%
                                  dplyr::mutate(W = as.numeric(tau_args$treatment_vec == 0))),
                           relevant_args))
    sbart_estimates <- estimate_sbart_tau(sbart_fit$yhat.train,
                                          sbart_fit$yhat.test,
                                          tau_args$treatment_vec)

    return(list(tau_hat = sbart_estimates$means_cate,
                variance_estimates = sbart_estimates$vars_cate,
                var_importance = NULL,
                fit_object = sbart_fit))
  }
}

#' @export
#' @rdname generate_tau
generate_tau.ensembleforest_args <- function(named_args,
                                             ...) {
  aug_data <- build_aug_data(named_args,
                             ...)
  fit_ensemble_forest(aug_data,
                      named_args$study_col,
                      named_args$covariate_col,
                      ...)
}

