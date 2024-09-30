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
                          treat_col,
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

  # symbols for columns
  site <- rlang::sym(site_col)
  treat <- rlang::sym(treat_col)
  outcome <- rlang::sym(outcome_col)
  dots <- list(...)

  if (is.null(covariate_col)) {
    exclude_col <- c(site_col, treat_col, outcome_col, drop_col)
    covariate_col <- colnames(trial_tbl)[-which(colnames(trial_tbl) %in% exclude_col)]
  }
  covariates <- rlang::syms(covariates_col)

  # prep feature tbl
  treat_vec <- trial_tbl[[treat_col]]
  outcome_vec <- trial_tbl[[outcome_col]]
  feature_tbl <- trial_tbl %>%
    dplyr::select(!!site, dplyr::all_of(covariate_col)) %>%
    {
      if (aggregation_method == "studyindicator") {
        fastDummies::dummy_cols(., select_columns = site_col, remove_selected_columns = TRUE)
      } else {
        .
      }
    }

  tau_hat <- estimate_tau(
    feature_tbl,
    treat_vec,
    outcome_vec,
    estimation_method,
    aggregation_method
  )

  # causal forest
  if (estimation_method == "causalforest") {
    if (aggregation_method == "studyindicator") {

    } else if (aggregation_method == "ensemble") {

    }
  }

}
