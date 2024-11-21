#' Generate Augmented Dataset Based on Selected Estimation Method
#'
#' @param named_args list. A list of pre-specified argument values from
#'  `multicate::estimate_cate()`.
#' @param site_val string, numeric, or factor. Unique identifier for primary site of interest
#' @param ... Additional arguments to be passed to `estimation_method`.
#'
#' @return A tbl with additional column `tau_hat` predicted for each observation based on a model
#' using the selected `estimation_method` trained on each site in turn. As a result, every
#' observation will have a separate row with a tau_hat prediction based on models trained on each
#' site (e.g., if the original data includes k sites, each with n observations, then the returned
#' tbl will have nk oservations)
generate_aug_tau <- function(named_args,
                             site_val,
                             ...) {
  UseMethod("generate_aug_tau")
}

#' @export
#' @rdname generate_aug_tau
generate_aug_tau.causalforest_args <- function(named_args,
                                               site_val,
                                               ...) {
  primary_site_args <- other_site_args <- named_args
  primary_site_args$trial_tbl <- primary_site_args$trial_tbl %>%
    dplyr::filter(!!rlang::sym(named_args[["site_col"]]) == site_val)
  other_site_args$trial_tbl <- other_site_args$trial_tbl %>%
    dplyr::filter(!!rlang::sym(named_args[["site_col"]]) != site_val)

  primary_tau_args <- prepare_cate_data(primary_site_args,
                                        "ensembleforest")
  other_tau_args <- prepare_cate_data(other_site_args,
                                      "ensembleforest")

  extra_args <- list(...)
  relevant_args <- extra_args[names(extra_args) %in% names(formals(grf::causal_forest))]
  primary_site_fit <- do.call(grf::causal_forest,
                              c(list(X = primary_tau_args$feature_tbl,
                                     W = primary_tau_args$treatment_vec,
                                     Y = primary_tau_args$outcome_vec),
                                relevant_args))

  primary_site_tau <- primary_site_args$trial_tbl %>%
    dplyr::mutate(tau_hat = c(primary_site_fit$predictions),
                  model_site = as.factor(site_val))
  other_site_tau <- other_site_args$trial_tbl %>%
    dplyr::mutate(tau_hat = predict(primary_site_fit,
                                    other_tau_args$feature_tbl)$predictions,
                  model_site = as.factor(site_val))

  dplyr::bind_rows(primary_site_tau, other_site_tau)
}

#' @export
#' @rdname generate_aug_tau
generate_aug_tau.xlearner_args <- function(named_args,
                                           site_val,
                                           ...) {
  primary_site_args <- named_args
  primary_site_args$trial_tbl <- primary_site_args$trial_tbl %>%
    dplyr::filter(!!rlang::sym(named_args[["site_col"]]) == site_val)

  primary_tau_args <- prepare_cate_data(primary_site_args,
                                        "ensembleforest")
  alldata_tau_args <- prepare_cate_data(named_args,
                                        "ensembleforest")

  extra_args <- list(...)
  relevant_args <- extra_args[names(extra_args) %in% names(formals(causalToolbox::X_RF))]
  xrf_fit <- do.call(causalToolbox::X_RF,
                     c(list(feat = primary_tau_args$feature_tbl,
                            tr = primary_tau_args$treatment_vec,
                            yobs = primary_tau_args$outcome_vec),
                       relevant_args))
  named_args$trial_tbl %>%
    dplyr::mutate(tau_hat = causalToolbox::EstimateCate(xrf_fit,
                                                        alldata_tau_args$feature_tbl),
                  model_site = as.factor(site_val))
}

#' @export
#' @rdname generate_aug_tau
generate_aug_tau.slearner_args <- function(named_args,
                                           site_val,
                                           ...) {
  primary_site_args <- named_args
  primary_site_args$trial_tbl <- primary_site_args$trial_tbl %>%
    dplyr::filter(!!rlang::sym(named_args[["site_col"]]) == site_val)

  primary_tau_args <- prepare_cate_data(primary_site_args,
                                        "ensembleforest")
  alldata_tau_args <- prepare_cate_data(named_args,
                                        "ensembleforest")

  extra_args <- list(...)
  relevant_args <- extra_args[names(extra_args) %in% names(formals(dbarts::bart))]
  relevant_args[["keeptrees"]] <- TRUE
  relevant_args[["verbose"]] <- FALSE

  sbart_fit <- do.call(dbarts::bart,
                       c(list(x.train = primary_tau_args$feature_tbl %>%
                                dplyr::mutate(W = primary_tau_args$treatment_vec),
                              y.train = primary_tau_args$outcome_vec),
                         relevant_args))

  observed_all <- predict(sbart_fit,
                          alldata_tau_args$feature_tbl %>%
                            dplyr::mutate(W = alldata_tau_args$treatment_vec))
  counterfactual_all <- predict(sbart_fit,
                                alldata_tau_args$feature_tbl %>%
                                  dplyr::mutate(W = as.numeric(alldata_tau_args$treatment_vec == 0)))

  named_args$trial_tbl %>%
    dplyr::mutate(tau_hat = estimate_sbart_tau(observed_all,
                                               counterfactual_all,
                                               alldata_tau_args$treatment_vec)$means_cate,
                  model_site = as.factor(site_val))
}
