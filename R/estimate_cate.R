#' Estimate Heterogeneous Treatment Effects Across Multiple Studies
#'
#' @description
#' `estimate_cate` is used to estimate conditional average treatment effect (CATE) from multiple
#' studies.
#'
#' @details
#' The resulting CATEs are of the form: E(Y(1) - Y(0)|X), where X includes covariates included in
#' the `covariate_col` argument. For a continuous outcome, this is the difference in the means
#' between treatment groups for a particular covariate profile X. For a binary outcome, this
#' translates to an estimate of the risk difference between the two treatment groups for X.
#'
#' This function relies on two methods: an estimation method and an aggregation method. Estimation
#' methods include an S-learner with BART (Hill, 2011; Künzel, 2019), and a causal forest (Athey et
#' al., 2019). Details on each approach can be found in the respective papers, as well as the R
#' packages `dbarts` and `grf`. Aggregation methods include the pooling with trial indicator method
#' and the ensemble forest (Brantner et al., 2024). All methods included in this package performed
#' well in simulations across various settings of cross-study heterogeneity and functional forms of
#' the CATE (Brantner et al., 2024).
#'
#' Variance estimates are available for all estimation methods when the pooling with trial indicator
#' aggregation method is used. Variance estimates are not yet available for the ensemble forest.
#'
#' Data from the studies being combined can come from randomized clinical trials, observational
#' studies, or both. The estimation methods can inherently handle confounding in treatment
#' assignment, and users can allow the methods to automatically address confounding.
#' If desired, propensity scores can be separately estimated and included in one of two ways: as a
#' covariate for S-learner with BART, or as the `W.hat` argument in the causal forest (see `grf`
#' documentation).
#'
#' @references
#' Hill, J. L. (2011). Bayesian Nonparametric Modeling for Causal Inference.
#' *Journal of Computational and Graphical Statistics, 20*(1), 217–240.
#' [DOI:10.1198/jcgs.2010.08162](http://www.tandfonline.com/doi/abs/10.1198/jcgs.2010.08162)
#'
#' Künzel, S. R., Sekhon, J. S., Bickel, P. J., & Yu, B. (2019). Metalearners for estimating
#' heterogeneous treatment effects using machine learning.
#' *Proceedings of the National Academy of Sciences, 116*(10), 4156–4165.
#'
#' Athey, S., Tibshirani, J., & Wager, S. (2019). Generalized random forests.
#' *The Annals of Statistics, 47*(2), 1148–1178.
#'
#' Brantner, C. L., Nguyen, T. Q., Tang, T., Zhao, C., Hong, H., & Stuart, E. A. (2024).
#' Comparison of methods that combine multiple randomized trials to estimate heterogeneous treatment
#' effects. *Statistics in Medicine.*
#'
#' @param trial_tbl tbl. A tbl containing columns for treatment, outcome, study ID, and any
#'  additional covariates of interest. All study data must be included in single tbl. Note that
#'  only two treatments can be considered and treatment must be coded as 0/1 (numeric).
#' @param estimation_method string. Single-study methods for estimating CATE (tau) for each
#'  observation. Available methods are "slearner" (using Bayesian Additive Regression Trees),
#'  and "causalforest".
#' @param aggregation_method string. Method for aggregating results across studies Available methods
#'  are:
#'    - "studyindicator": Pool all data together but keep study ID as an indicator variable and
#'    includes them as covariates in the single-study method selected in `estimation_method`.
#'    - "ensembleforest": Fit the `estimation_method` within each study, apply each model to all
#'    individuals across all studies, then fit ensemble random forest to augmented data.
#'    - "studyspecific": Fit the `estimation_method` separately for each study and report out
#'    study-specific estimates.
#' @param study_col string. Name of the column in `trial_tbl` with study ID.
#' @param treatment_col string. Name of the column in `trial_tbl` with treatment assignment.
#' @param outcome_col string. Name of the column in `trial_tbl` with outcome values.
#' @param covariate_col character vector. Name(s) of columns in `trial_tbl` to be included as
#'  model covariates. Defaults to NULL.
#' @param drop_col character vector. Name(s) of columns in `trial_tbl` to be excluded from
#'  model covariates. Defaults to NULL.
#' @param ... Arguments to be passed to `estimation_method` and/or `aggregation_method`. Note the
#'  following exceptions:
#'    * `estimation_method` = "slearner"
#'      * \link[dbarts:bart]{dbarts::bart()} argument `keeptrees` set to TRUE
#'      * \link[dbarts:bart]{dbarts::bart()} argument `verbose` set to FALSE when `aggregation_method` is
#'      "ensembleforest"
#'    * `aggregation_method` = "ensembleforest"
#'      * \link[ranger:ranger]{ranger::ranger()} argument `importance` set to "impurity"
#'      * \link[ranger:ranger]{ranger::ranger()} argument `keep.inbag` set to TRUE
#'
#' @return An object of class "cate" is a list containing the following elements:
#'  * `estimation_method`: the type of estimation method used
#'  * `aggregation_method`: the type of aggregation method used
#'  * `model`: `trial_tbl` with additional columns
#'    * `tau_hat`: conditional average treatment effect for each observation
#'    * `variance_estimates`: variance estimate for each tau_hat (not available when
#'    `aggregation_method` set to "ensembleforest)
#'  * `var_importance`: a measure of how involved each variable was in creating the forest
#'  (see grf and ranger documentation for more details on calculation)
#'  * `study_col`: name of study ID column
#'  * `treatment_col`: name of treatment column
#'  * `outcome_col`: name of outcome column
#'  * `covariate_col`: name(s) of covariate columns
#'  * `estimation_object`: object(s) from \link[grf:causal_forest]{grf::causal_forest}
#'  or \link[dbarts:bart]{dbarts::bart}, according to the `estimation_method` selected.
#'
#' @example inst/examples/example-estimate_cate.R
#' @export
estimate_cate <- function(trial_tbl,
                          estimation_method = c("slearner",
                                                "causalforest"),
                          aggregation_method = c("studyindicator",
                                                 "ensembleforest",
                                                 "studyspecific"),
                          study_col,
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

  assert_column_class(trial_tbl, treatment_col, c("numeric", "integer"))

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


