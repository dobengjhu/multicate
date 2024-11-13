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
                          ...
                          ) {
  # assertions on methods
  estimation_method <- match.arg(estimation_method)
  aggregation_method <- match.arg(aggregation_method)

  # assertions on trial_tbl
  # TODO: Assert site, treatment, and outcome cols are not in covariate_col or drop_col

  if (is.null(covariate_col)) {
    exclude_col <- c(site_col, treatment_col, outcome_col, drop_col)
    covariate_col <- colnames(trial_tbl)[-which(colnames(trial_tbl) %in% exclude_col)]
  }

  named_args <- list(trial_tbl = trial_tbl,
                     site_col = site_col,
                     treatment_col = treatment_col,
                     outcome_col = outcome_col,
                     covariate_col = covariate_col,
                     estimation_method = estimation_method)
  class(named_args) <- c(glue::glue("{estimation_method}_args"),
                         glue::glue("{aggregation_method}_args"))

  tau_list <- generate_tau(named_args,
                           ...)
  return(list(tau_tbl = trial_tbl %>%
                dplyr::mutate(tau_hat = tau_list$tau_hat),
              var_importance = tau_list$var_importance))

}


