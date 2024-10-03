#' Title
#'
#' @param named_args
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
generate_tau <- function(named_args,
                         ...) {
  UseMethod("generate_tau")
}


#' Title
#'
#' @param named_args
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
    tau$predictions
  } else if (named_args$estimation_method == "xlearner") {
    extra_args <- list(...)
    relevant_args <- extra_args[names(extra_args) %in% names(formals(causalToolbox::X_RF))]
    xrf_fit <- do.call(causalToolbox::X_RF,
                       c(list(feat = tau_args$feature_tbl,
                              tr = tau_args$treatment_vec,
                              yobs = tau_args$outcome_vec),
                         relevant_args))
    causalToolbox::EstimateCate(xrf_fit,
                                tau_args$feature_tbl)
  } else if (named_args$estimation_method == "slearner") {
    extra_args <- list(...)
    relevant_args <- extra_args[names(extra_args) %in% names(formals(causalToolbox::S_RF))]
    srf_fit <- do.call(causalToolbox::S_RF,
                       c(list(feat = tau_args$feature_tbl,
                              tr = tau_args$treatment_vec,
                              yobs = tau_args$outcome_vec),
                         relevant_args))
    causalToolbox::EstimateCate(srf_fit,
                                tau_args$feature_tbl)
  }
}

#' Title
#'
#' @param named_args
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
generate_tau.ensembleforest_args <- function(named_args,
                                             ...) {
  aug_data <- build_aug_data(named_args,
                             ...)
  fit_ensemble_forest(aug_data,
                      named_args$site_col,
                      named_args$covariate_col,
                      ...)
}
