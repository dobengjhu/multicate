#' Title
#'
#' @param trial_tbl
#' @param estimation_method
#' @param aggregation_method
#' @param site_col
#' @param treat_col
#' @param outcome_col
#' @param covariate_col
#' @param drop_col
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
estimate_cate <- function(trial_tbl,
                          estimation_method = c("slearner", "xlearner", "causalforest"),
                          aggregation_method = c("studyindicator", "ensembleforest"),
                          site_col,
                          treatment_col,
                          outcome_col,
                          covariate_col = NULL,
                          drop_col = NULL,
                          incl_cfobject = FALSE,
                          ...
                          ) {
  # assertions on methods
  estimation_method <- match.arg(estimation_method)
  aggregation_method <- match.arg(aggregation_method)

  # assertions on trial_tbl
  # TODO: Assert site, treatment, and outcome cols are not in covariate_col or drop_col
  # TODO: Assert that treatment variable is 0/1
  # TODO: Assert that treatment variable doesn't have more than 2 levels
  # TODO: Assert incl_cfobject is TRUE or FALSE

  if (is.null(covariate_col)) {
    exclude_col <- c(site_col, treatment_col, outcome_col, drop_col)
    covariate_col <- colnames(trial_tbl)[-which(colnames(trial_tbl) %in% exclude_col)]
  }

  named_args <- list(trial_tbl = trial_tbl,
                     site_col = site_col,
                     treatment_col = treatment_col,
                     outcome_col = outcome_col,
                     covariate_col = covariate_col,
                     estimation_method = estimation_method,
                     incl_cfobject = incl_cfobject)
  class(named_args) <- c(glue::glue("{estimation_method}_args"),
                         glue::glue("{aggregation_method}_args"))

  tau_list <- generate_tau(named_args,
                           ...)

  cate_object <- list(
    estimation_method = estimation_method,
    aggregation_method = aggregation_method,
    model = trial_tbl %>%
      dplyr::mutate(tau_hat = tau_list$tau_hat,
                    variance_estimates = tau_list$variance_estimates),
    var_importance = tau_list$var_importance,
    site_col = site_col,
    treatment_col = treatment_col,
    outcome_col = outcome_col,
    covariate_col = covariate_col
  )

  if (incl_cfobject) {
    cate_object$causalforest <- tau_list$causalforest_obj
  }

  class(cate_object) <- "cate"
  return(cate_object)
}


