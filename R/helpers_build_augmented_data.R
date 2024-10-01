predict_tau <- function(named_args,
                        site_val,
                        ...) {
  UseMethod("predict_tau")
}

#' Title
#'
#' @param site_val
#' @param named_args
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
predict_tau.causalforest_args <- function(named_args,
                                          site_val,
                                          ...) {
  browser()
  primary_site_args <- other_site_args <- named_args
  primary_site_args$trial_tbl <- primary_site_args$trial_tbl %>%
    dplyr::filter(!!rlang::sym(named_args[["site_col"]]) == site_val)
  other_site_args$trial_tbl <- other_site_args$trial_tbl %>%
    dplyr::filter(!!rlang::sym(named_args[["site_col"]]) != site_val)

  primary_tau_args <- prepare_cate_data(primary_site_args,
                                        "ensembleforest")
  other_tau_args <- prepare_cate_data(other_site_args,
                                      "ensembleforest")

  primary_site_fit <- grf::causal_forest(X = primary_tau_args$feature_tbl,
                                         W = primary_tau_args$treatment_vec,
                                         Y = primary_tau_args$outcome_vec,
                                         ...)

  primary_site_tau <- primary_site_args$trial_tbl %>%
    dplyr::mutate(tau_hat = c(primary_site_fit$predictions),
                  model_site = as.factor(site_val))
  other_site_tau <- other_site_args$trial_tbl %>%
    dplyr::mutate(tau_hat = predict(primary_site_fit,
                                    other_tau_args$feature_tbl)$predictions,
                  model_site = as.factor(site_val))

  dplyr::bind_rows(primary_site_tau, other_site_tau)
}
