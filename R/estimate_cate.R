#' Estimate Heterogeneous Treatment Effects Across Multiple Randomized Control Trials
#'
#' @description
#' `estimate_cate` is used to estimate conditional average treatment effect (CATE) from multiple
#' studies.
#'
#' @param trial_tbl tbl. A tbl containing columns for treatment, outcome, site ID, and any
#'  additional covariates of interest. All site data must be included in single tbl. Note that
#'  only two treatments can be considered and treatment must be coded as 0/1 (numeric).
#' @param estimation_method string. Single-study methods for estimating CATE (tau) for each
#'  observation. Available methods are "slearner" (using Bayesian Additive Regression Trees),
#'  "xlearner", and "causalforest".
#' @param aggregation_method string. Method for aggregating results across sites. Available methods
#'  are:
#'    - "studyindicator": Pool all data together but keep study ID as an indicator variable and
#'    includes them as covariates in the single-study method selected in `estimation_method`.
#'    - "ensembleforest": Fit the `estimation_method` within each study, apply each model to all
#'    individuals across all sites, then fit ensemble random forest to augmented data.
#' @param site_col string. Name of the column in `trial_tbl` with site ID.
#' @param treatment_col string. Name of the column in `trial_tbl` with treatment assignment.
#' @param outcome_col string. Name of the column in `trial_tbl` with outcome values.
#' @param covariate_col character vector. Name(s) of columns in `trial_tbl` to be included as
#'  model covariates. Defaults to NULL.
#' @param drop_col character vector. Name(s) of columns in `trial_tbl` to be excluded from
#'  model covariates. Defaults to NULL.
#' @param incl_cfobject logical. When TRUE, if `estimation_method` is "causalforest" the causal
#'  forest object is saved and returned.
#' @param ... Arguments to be passed to `estimation_method` and/or `aggregation_method`. Note the
#'  following exceptions:
#'    - `estimation_method` = "slearner"
#'      - \link[dbarts:bart]{dbarts::bart()} argument `keeptrees` set to TRUE
#'      - \link[dbarts:bart]{dbarts::bart()} argument `verbose` set to FALSE when `aggregation_method` is
#'      "ensembleforest"
#'    - `aggregation_method` = "ensembleforest"
#'      - \link[ranger:ranger]{ranger::ranger()} argument `importance` set to "impurity"
#'      - \link[ranger:ranger]{ranger::ranger()} argument `keep.inbag` set to TRUE
#'
#' @return An object of class "cate" is a list containing the following elements:
#'  - `estimation_method`: the type of estimation method used
#'  - `aggregation_method`: the type of aggregation method used
#'  - `model`: `trial_tbl` with additional columns
#'    - `tau_hat`: conditional average treatment effect for each observation
#'    - `variance_estimates`: variance estimate for each tau_hat (not available when
#'    `estimation_method` set to "xlearner" or `aggregation_method` set to "ensembleforest)
#'  - `var_importance`: TBD
#'  - `site_col`: name of site ID column
#'  - `treatment_col`: name of treatment column
#'  - `outcome_col`: name of outcome column
#'  - `covariate_col`: name(s) of covariate columns
#'  - `causalforest`: Trained causal forest object from \link[grf:causal_forest]{causal_forest} (If
#'    `estimation_method` is set to "causalforest" and `incl_cfobject` is TRUE)
#'
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
  # TODO: Assert outcome is numeric
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


