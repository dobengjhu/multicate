estimate_tau <- function(feature_tbl,
                         treatment_vec,
                         outcome_vec,
                         estimation_method,
                         aggregation_method,
                         ...) {
  arg_val <- list(feature_tbl = feature_tbl,
                  treatment_vec = treatment_vec,
                  outcome_vec = outcome_vec,
                  estimation_method = estimation_method)

  if (aggregation_method == "studyindicator") {
    estimate_tau.studyindicator(arg_val,
                                ...)
  } else if (aggregation_method == "ensembleforest") {
    estimate_tau.ensembleforest(arg_val,
                                ...)
  }
}

estimate_tau.studyindicator <- function(arg,
                                        ...) {
  cli::cli_alert_info("Using ensemble forest method...")
  if (arg[["estimation_method"]] == "causalforest") {
    tau <- grf::causal_forest(X = arg[["feature_tbl"]],
                              W = arg[["treatment_vec"]],
                              Y = arg[["outcome_vec"]],
                              ...)
    tau$predictions
  } else if (arg[["estimation_method"]] == "xlearner") {
    xrf_model <- causalToolbox::X_RF(feat = arg[["feature_tbl"]],
                                     tr = arg[["treatment_vec"]],
                                     yobs = arg[["outcome_vec"]],
                                     ...)
    tau <- causalToolbox::EstimateCate(xrf_model,
                                       arg[["feature_tbl"]])
    tau
  } else if (arg[["estimation_method"]] == "slearner") {
    srf_model <- causalToolbox::S_RF(feat = arg[["feature_tbl"]],
                                     tr = arg[["treatment_vec"]],
                                     yobs = arg[["outcome_vec"]],
                                     ...)
    tau <- causalToolbox::EstimateCate(srf_model,
                                       arg[["feature_tbl"]])
    tau
  }
}

estimate_tau.ensembleforest <- function(arg) {

}
