#' Predict Heterogeneous Treatment Effects Based on Estimated CATEs
#'
#' @description
#' `predict.cate` is used to produce a prediction of the conditional average treatment effect (CATE)
#' based on models fit to data from multiple studies/sites.
#'
#' @details
#' The predict function uses CATE models fit to multiple studies (objects resulting from
#' \link{estimate_cate}) to form prediction intervals for CATEs in a new, target setting (e.g., a
#' group of patients in an electronic health record system). This function leverages two-stage meta-
#' analysis with prediction intervals for all covariate profiles (\eqn{\mathbf{X}^*}) of interest in
#' the target setting.
#'
#' The first stage of the two-stage meta-analysis is completed by fitting models to each study individually
#' to estimate the CATE per study using the \link{estimate_cate} function with
#' the "studyspecific" aggregation method. From this first stage, we can obtain estimates of the CATE for a
#' given covariate profile in the target setting in each study i: \eqn{\hat{\tau}_i(\mathbf{X}^*)}, as well as variance
#' estimates of those CATEs.
#'
#' The second stage of the two-stage meta-analysis is replicated for every covariate profile
#' \eqn{\mathbf{X}^*} of interest. In this stage, we fit a random effects meta-analysis under the
#' distributional assumptions:
#'
#' \deqn{\hat{\tau}_i(\mathbf{X}^*) \sim
#' N(\tau_i(\mathbf{X}^*),
#' {s_i}^2(\mathbf{X}^*))}
#'
#' where \eqn{\hat{\tau}_i(\mathbf{X}^*)}
#' is the estimated CATE for \eqn{\mathbf{X}^*} in study i,
#' \eqn{\tau_i(\mathbf{X}^*)} is the true CATE for
#' \eqn{\mathbf{X}^*} in study i, and
#' \eqn{{s_i}^2(\mathbf{X}^*)} is the within-study variability estimate of the CATE for
#' \eqn{\mathbf{X}^*}; and
#'
#' \deqn{\tau_i(\mathbf{X}^*) \sim N(\tau(\mathbf{X}^*), \theta^2(\mathbf{X}^*))}
#'
#' where \eqn{\tau(\mathbf{X}^*)}
#' is the true average CATE for \eqn{\mathbf{X}^*} across all studies, and
#' \eqn{\theta^2(\mathbf{X}^*)} is the between-study variance.
#'
#' From this two-stage meta-analysis, the prediction interval for the CATE for covariate profile
#' \eqn{\mathbf{X}^*} in the target setting represents a range of potential values that the CATE
#' may be in the new setting. This prediction interval is of the form:
#'
#' \deqn{\hat{\tau}(\mathbf{X}^*) \pm t_{K-2} \sqrt{SE(\hat{\tau}(\mathbf{X}^*)^2 +
#' \hat{\theta}^2(\mathbf{X}^*)}}
#'
#' where \eqn{\hat{\tau}(\mathbf{X}^*)} is the inverse variance-weighted average of the
#' \eqn{\hat{\tau}_i(\mathbf{X}^*)} across studies, \eqn{t_{K-2}} is the t-statistic with K-2 degrees
#' of freedom (where K is the total number of studies), \eqn{SE(\hat{\tau}(\mathbf{X}^*)^2} is the
#' estimated variance of \eqn{\hat{\tau}(\mathbf{X}^*)}, and \eqn{\hat{\theta}^2(\mathbf{X}^*)} is
#' the estimated between-study variance, calculated using REML. For more details, see Brantner et
#' al., (Under review) and Riley et al. (2021).
#'
#' The predict function can only be run on `cate` objects using the "studyspecific" aggregation
#' method. Any estimation method (i.e., causal forest or S-learner with BART) can be used.
#'
#' The `newdata_tbl` can consist of any set of profiles for which the researcher would like to
#' create prediction intervals. The assumption with this approach is that the profiles in the target
#' setting can be found within the covariate distribution of at least one of the studies used to
#' estimate the CATE (i.e., positivity). CATE prediction intervals will be constructed separately
#' for each covariate profile, represented by a row in the newdata_tbl.
#'
#' @references
#' Brantner CL, Nguyen TQ, Parikh H, Zhao C, Hong H, Stuart EA. Precision mental health: Predicting
#' heterogeneous treatment effects for depression through data integration. _Under review._
#'
#' Riley, RD, Debray TP, Morris TP, Jackson D. The Two-Stage Approach to IPD Meta-Analysis. In
#' Individual Participant Data Meta-Analysis (pp 87-125). (2021)
#'
#' @importFrom dplyr n
#' @importFrom data.table :=
#' @importFrom stats var qt
#' @importFrom rlang .data
#' @param object list. An object resulting from \link{estimate_cate}.
#' @param newdata_tbl tbl. A tbl containing columns for treatment, outcome, study ID, and any
#'  additional covariates used to generate `cate_obj`. All study data must be included in single
#'  tbl. Note that only two treatments can be considered and treatment must be coded as 0/1
#'  (numeric).
#' @param alpha numeric. Significance level for confidence intervals. Defaults to 0.05 for 95% CIs.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A tbl including all columns and data from `newdata_tbl`, with additional columns
#' including a mean predicted CATE and associated 100(1 - \eqn{\alpha})% prediction interval.
#' @export
#'
#' @example inst/examples/example-predict_cate.R
#' @export
predict.cate <- function(object,
                         newdata_tbl,
                         alpha = 0.05,
                         ...) {

  assertthat::assert_that(
    inherits(object, "cate"),
    msg = "use only with \"cate\" objects"
  )

  assertthat::assert_that(
    any(class(newdata_tbl) %in% c("tbl", "data.frame")),
    msg = "newdata_tbl` must be a tibble or data.frame."
  )

  assertthat::assert_that(
    object$aggregation_method == "studyspecific",
    msg = "Aggregation method must be 'studyspecific'."
  )

  assertthat::assert_that(
    is.numeric(alpha),
    msg = "`alpha` must be a numeric value between 0 and 1."
  )

  assertthat::assert_that(
    dplyr::between(alpha, 0, 1),
    msg = "`alpha` must be a numeric value between 0 and 1."
  )

  model <- object$model
  estimation_object <- object$estimation_object
  W <- rlang::sym(object$treatment_col)
  Y <- rlang::sym(object$outcome)
  if (!is.na(object$study_col)) {
    S <- rlang::sym(object$study_col)
    K <- model %>%
      dplyr::distinct(!!S) %>%
      nrow()
  } else {
    K <- 1
  }

  assertthat::assert_that(
    K > 2,
    msg = "Insufficient degrees of freedom; original model must include at least 3 studies."
  )

  required_colnames <- model %>%
    dplyr::select(!!W, !!!rlang::syms(object$covariate_col)) %>%
    colnames()

  assertthat::assert_that(
    all(required_colnames %in% colnames(newdata_tbl)),
    msg = glue::glue("New data must include same treatment and covariate columns used \\
                     to fit original model")
  )

  original_colnames <- setdiff(colnames(newdata_tbl), as.character(S))

  newfeat <- newdata_tbl %>%
    {
      if (object$estimation_method == "causalforest") {
        dplyr::select(., dplyr::all_of(object$covariate_col))
      } else {
        dplyr::select(., !!W, dplyr::all_of(object$covariate_col))
      }
    }

  if (object$estimation_method == "causalforest") {
    preddata_tbl <- purrr::map_df(
      .x = estimation_object,
      .f = function(.x) {
        predict_causal_forest <- get("predict.causal_forest", envir = asNamespace("grf"))
        relevant_args <- object$extra_args[names(object$extra_args) %in% names(formals(predict_causal_forest))]
        newdata_pred <- do.call(predict_causal_forest,
                                c(list(object = .x,
                                       newdata = newfeat,
                                       estimate.variance = TRUE),
                                  relevant_args))

        newdata_tbl %>%
          dplyr::mutate(tau_hat = newdata_pred$predictions,
                        tau_var = newdata_pred$variance.estimates)
      }
    )
  } else if (object$estimation_method == "slearner") {
    preddata_tbl <- purrr::map_df(
      .x = estimation_object,
      .f = function(.x) {
        newfeat_counterfactual <- newfeat %>%
          dplyr::mutate(!!W := as.numeric(!!W == 0))

        predict_bart <- get("predict.bart", envir = asNamespace("dbarts"))
        relevant_args <- object$extra_args[names(object$extra_args) %in% names(formals(predict_bart))]
        newdata_pred <- do.call(predict_bart,
                                c(list(object = .x,
                                       newdata = newfeat),
                                  relevant_args))
        newdata_pred_counterfactual <- do.call(predict_bart,
                                               c(list(object = .x,
                                                      newdata = newfeat_counterfactual),
                                                 relevant_args))

        tau_list <- estimate_sbart_tau(newdata_pred,
                                       newdata_pred_counterfactual,
                                       newdata_tbl[[as.character(W)]])
        newdata_tbl %>%
          dplyr::mutate(tau_hat = tau_list$means_cate,
                        tau_var = tau_list$vars_cate)
      }
    )
  }

  preddata_tbl <- preddata_tbl %>%
    dplyr::group_by(!!!rlang::syms(original_colnames)) %>%
    dplyr::mutate(.profileid = dplyr::cur_group_id())

  preddata_tbl %>%
    dplyr::group_split() %>%
    purrr::map(
      ~ metafor::rma(yi = tau_hat,
                     vi = tau_var,
                     data = .x,
                     method = "REML")
    ) %>%
    purrr::map(
      ~ predict(.x,
                pi.type = "Riley",
                level = 100 * (1 - alpha))
    ) %>%
    purrr::imap(
      function(.x, .y) {
        tibble::tibble(
          tau_predicted = .x$pred,
          pi_lower = .x$pi.lb,
          pi_upper = .x$pi.ub,
          .profileid = .y
        )
      }
    ) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::left_join(preddata_tbl, ., by = ".profileid") %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(.data$.profileid, .data$tau_hat, .data$tau_var)) %>%
    dplyr::distinct() %>%
    dplyr::left_join(newdata_tbl, ., by = original_colnames)
}
