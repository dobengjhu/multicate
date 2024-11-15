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

    var_import <- tibble::tibble(
      variable = colnames(tau_args$feature_tbl),
      importance = as.numeric(grf::variable_importance(tau))
    )

    return(list(tau_hat = as.numeric(tau$predictions),
                var_importance = var_import))
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
                var_importance = NULL))
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

    return(list(tau_hat = estimate_sbart_tau(sbart_fit$yhat.train.mean,
                                             sbart_fit$yhat.test.mean,
                                             tau_args$treatment_vec),
                var_importance = NULL))
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

