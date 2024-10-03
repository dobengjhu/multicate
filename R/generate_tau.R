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

  if (named_args[["estimation_method"]] == "causalforest") {
    tau <- grf::causal_forest(X = tau_args[["feature_tbl"]],
                              W = tau_args[["treatment_vec"]],
                              Y = tau_args[["outcome_vec"]],
                              ...)
    tau$predictions
  } else if (named_arg[["estimation_method"]] == "xlearner") {
    xrf_fit <- causalToolbox::X_RF(feat = tau_args[["feature_tbl"]],
                                   tr = tau_args[["treatment_vec"]],
                                   yobs = tau_args[["outcome_vec"]],
                                   ...)
    causalToolbox::EstimateCate(xrf_fit,
                                tau_args[["feature_tbl"]])
  } else if (named_args[["estimation_method"]] == "slearner") {
    srf_fit <- causalToolbox::S_RF(feat = tau_args[["feature_tbl"]],
                                   tr = tau_args[["treatment_vec"]],
                                   yobs = tau_args[["outcome_vec"]],
                                   ...)
    causalToolbox::EstimateCate(srf_fit,
                                tau_args[["feature_tbl"]])
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
