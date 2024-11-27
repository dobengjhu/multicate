#' Estimate Heterogeneous Treatment Effects Across Multiple Studies
#'
#' @description
#' `estimate_cate` is used to estimate conditional average treatment effect (CATE) from multiple
#' studies.
#'
#' @param trial_tbl tbl. A tbl containing columns for treatment, outcome, study ID, and any
#'  additional covariates of interest. All study data must be included in single tbl. Note that
#'  only two treatments can be considered and treatment must be coded as 0/1 (numeric).
#' @param estimation_method string. Single-study methods for estimating CATE (tau) for each
#'  observation. Available methods are "slearner" (using Bayesian Additive Regression Trees),
#'  "xlearner", and "causalforest".
#' @param aggregation_method string. Method for aggregating results across studies Available methods
#'  are:
#'    - "studyindicator": Pool all data together but keep study ID as an indicator variable and
#'    includes them as covariates in the single-study method selected in `estimation_method`.
#'    - "ensembleforest": Fit the `estimation_method` within each study, apply each model to all
#'    individuals across all studies, then fit ensemble random forest to augmented data.
#' @param study_col string. Name of the column in `trial_tbl` with study ID.
#' @param treatment_col string. Name of the column in `trial_tbl` with treatment assignment.
#' @param outcome_col string. Name of the column in `trial_tbl` with outcome values.
#' @param covariate_col character vector. Name(s) of columns in `trial_tbl` to be included as
#'  model covariates. Defaults to NULL.
#' @param drop_col character vector. Name(s) of columns in `trial_tbl` to be excluded from
#'  model covariates. Defaults to NULL.
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
#'  - `study_col`: name of study ID column
#'  - `treatment_col`: name of treatment column
#'  - `outcome_col`: name of outcome column
#'  - `covariate_col`: name(s) of covariate columns
#'  - `estimation_object`: object from \link[grf:causal_forest]{grf::causal_forest},
#'  \link[causalToolbox:X_RF]{causalToolbox::X_RF}, or \link[dbarts:bart]{dbarts::bart}, according
#'  to the `estimation_method` selected.
#'
#' @export
#'
#' @examples
estimate_cate <- function(trial_tbl,
                          estimation_method = c("slearner", "xlearner", "causalforest"),
                          aggregation_method = c("studyindicator", "ensembleforest"),
                          study_col,
                          treatment_col,
                          outcome_col,
                          covariate_col = NULL,
                          drop_col = NULL,
                          ...
                          ) {
  browser()
  # assertions on methods
  estimation_method <- match.arg(estimation_method)
  aggregation_method <- match.arg(aggregation_method)

  # assertions on trial_tbl
  assert_column_names_exist(trial_tbl, study_col, treatment_col, outcome_col)

  if (!is.null(covariate_col)) {
    assert_column_names_exist(trial_tbl, covariate_col)
  }

  if (!is.null(drop_col)) {
    assert_column_names_exist(trial_tbl, drop_col)
  }

  assertthat::assert_that(
    !(any(c(study_col, treatment_col, outcome_col) %in% c(covariate_col, drop_col))),
    msg = "`covariate_col` and `drop_col` cannot include the study, treatment, and/or outcome columns."
  )

  assertthat::assert_that(
    length(setdiff(unique(trial_tbl[[treatment_col]]), c(0,1))) == 0,
    msg = "Treatment values must be 0, 1, or NA."
  )

  assert_column_class(trial_tbl, outcome_col, "numeric")


  if (is.null(covariate_col)) {
    exclude_col <- c(study_col, treatment_col, outcome_col, drop_col)
    covariate_col <- colnames(trial_tbl)[-which(colnames(trial_tbl) %in% exclude_col)]
  }

  named_args <- list(trial_tbl = trial_tbl,
                     study_col = study_col,
                     treatment_col = treatment_col,
                     outcome_col = outcome_col,
                     covariate_col = covariate_col,
                     estimation_method = estimation_method)
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
    study_col = study_col,
    treatment_col = treatment_col,
    outcome_col = outcome_col,
    covariate_col = covariate_col,
    extra_args = list(...),
    estimation_object = tau_list$fit_object
  )
  class(cate_object) <- "cate"
  return(cate_object)
}


