#' Summarizing CATE Model Fits
#'
#' @importFrom stats sd median
#' @param object list. An object of class "cate", usually a result of a call to \link{estimate_cate}.
#' @param tauhat_column string. When `aggregation_method` is "ensembleforest", name of the column in
#'  `object$model` that has the estimated CATEs. Default is "tau_hat".
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A list of summary statistics and tables based on the information given in `object`,
#' including
#' * `estimation method` : The estimation method used to estimate the CATEs.
#' * `aggregation method` : The aggregation method used to estimate the CATEs.
#' * `vi_table` : The variable importance table. When the `aggregation_method` is "studyspecific",
#' a matrix of variable importance values based on the study-specific models is provided.
#' * `ate` : The overall average treatment effect across all patients and all studies. When the
#' `estimation_method` is "causalforest", this is estimated using the
#' \link[grf:average_treatment_effect]{grf::average_treatment_effect} function. When the
#' `estimation_method` is "slearner", this is estimated by averaging the treatment effect across
#' patients, accounting for counterfactual assignments. When the `aggregation_method` is
#' "ensembleforest" or "studyspecific", a simple arithmetic mean of estimated CATEs is provided and
#' the standard error is `NA`.
#' * `studycate` : A table of the minimum, median, and maximum CATE values for patients from each
#' study included in the model.
#'
#' @example inst/examples/example-summary_cate.R
#' @export
summary.cate <- function(object,
                         tauhat_column = "tau_hat",
                         ...) {

  assertthat::assert_that(
    "cate" %in% class(object),
    msg = "Object must be of class cate"
  )

  if (object$aggregation_method == "ensembleforest") {
    assert_column_names_exist(object$model, tauhat_column)
  }

  summary_list <- list()
  summary_list$estimation_method <- object$estimation_method
  summary_list$aggregation_method <- object$aggregation_method

  if (object$aggregation_method %in% c("ensembleforest", "studyspecific")) {
    mean_tau_hat <- object$model %>%
      dplyr::summarise(mean_tau_hat = mean(!!rlang::sym(tauhat_column))) %>%
      dplyr::pull()
    summary_list$ate <- c(mean_tau_hat, NA)
  } else {
    if (object$estimation_method == "causalforest") {
      summary_list$ate <- grf::average_treatment_effect(object$estimation_object,
                                                        target.sample = "all")
    } else {
      sbart <- object$estimation_object
      w_fac <- ifelse(object$model[[object$treatment_col]] == 1, 1, -1)
      diffs <- sweep(sbart$yhat.train - sbart$yhat.test,
                     2,
                     w_fac,
                     '*')
      mean_diffs <- apply(diffs, 1, mean)
      summary_list$ate <- c(mean(mean_diffs), sd(mean_diffs))
    }
  }
  names(summary_list$ate) <- c("Estimate", "Std. Error")

  if (object$aggregation_method == "studyspecific") {
    summary_list$vi_table <- object$var_importance %>%
      purrr::reduce(dplyr::left_join, by = "variable")
    variable_names <- summary_list$vi_table$variable
    study_names <- names(object$var_importance)

    summary_list$vi_table <- summary_list$vi_table %>%
      dplyr::select(-.data$variable) %>%
      as.matrix()

    rownames(summary_list$vi_table) <- variable_names
    colnames(summary_list$vi_table) <- study_names
  } else {
    summary_list$vi_table <- object$var_importance %>%
      tidyr::pivot_wider(names_from = "variable",
                         values_from = "importance") %>%
      as.matrix()
    rownames(summary_list$vi_table) <- "Value"
  }

  summary_list$studycate <- object$model %>%
    dplyr::group_by(!!rlang::sym(object$study_col)) %>%
    dplyr::summarise(`Minimum CATE` = min(.data$tau_hat),
                     `Median CATE` = median(.data$tau_hat),
                     `Maximum CATE` = max(.data$tau_hat)) %>%
    dplyr::ungroup() %>%
    tibble::column_to_rownames(object$study_col) %>%
    as.matrix()

  if (object$aggregation_method == "studyspecific") {
    rownames(summary_list$studycate)
  }

  class(summary_list) <- "summary.cate"
  summary_list
}

#' @describeIn summary.cate Print the summary of a CATE model
#'
#' @param x list. An object of class `summary.cate`.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @export
print.summary.cate <- function(x,
                               ...) {
  dots <- list(...)

  if ("digits" %in% names(dots)) {
    digits = dots$digits
  } else {
    digits = max(3L, getOption("digits") - 3L)
  }

  cat("\nEstimation Method: ", x$estimation_method,
      "\nAggregation Method: ", x$aggregation_method,
      "\n", sep = "")

  cat("\nVariable Importance:\n")
  print(x$vi_table)

  cat("\nOverall ATE:\n")
  print(x$ate, digits = digits)

  cat("\nStudy-Specific ATE:\n")
  print(x$studycate, digits = digits)
}
