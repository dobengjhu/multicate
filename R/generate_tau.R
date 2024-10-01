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
  browser()
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
generate_tau.studyindicator <- function(named_args,
                                        ...) {
  tau_args <- prepare_cate_data(
    named_args,
    "studyindicator"
  )

  cli::cli_alert_info("Using study indicator method...")
  if (named_args[["estimation_method"]] == "causalforest") {
    tau <- grf::causal_forest(X = tau_args[["feature_tbl"]],
                              W = tau_args[["treatment_vec"]],
                              Y = tau_args[["outcome_vec"]],
                              ...)
    tau$predictions
  } else if (named_arg[["estimation_method"]] == "xlearner") {
    xrf_model <- causalToolbox::X_RF(feat = tau_args[["feature_tbl"]],
                                     tr = tau_args[["treatment_vec"]],
                                     yobs = tau_args[["outcome_vec"]],
                                     ...)
    causalToolbox::EstimateCate(xrf_model,
                                tau_args[["feature_tbl"]])
  } else if (named_args[["estimation_method"]] == "slearner") {
    srf_model <- causalToolbox::S_RF(feat = tau_args[["feature_tbl"]],
                                     tr = tau_args[["treatment_vec"]],
                                     yobs = tau_args[["outcome_vec"]],
                                     ...)
    causalToolbox::EstimateCate(srf_model,
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
generate_tau.ensembleforest <- function(named_args,
                                        ...) {
  browser()
  cli::cli_alert_info("Using ensemble forest method...")

  # if (named_args$estimation_method == "causalforest") {
  #   class(named_args) <- "causalforest_args"
  # } else if (named_args$estimation_method == "xlearner") {
  #   class(named_args) <- "xlearner_args"
  # } else if (named_args$estimation_method == "slearner") {
  #   class(named_args) <- "slearner_args"
  # }

  aug_data <- build_aug_data(named_args,
                             ...)
  fit_ensemble_forest(aug_data,
                      named_args$site_col,
                      named_args$covariate_col,
                      ...)
}
