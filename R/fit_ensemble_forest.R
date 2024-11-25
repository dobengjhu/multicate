#' Fit Ensemble Forest
#'
#' @param aug_data tbl.
#' @param study_col string. Name of the column in `trial_tbl` with study ID.
#' @param covarite_col character vector. Name(s) of columns in `trial_tbl` to be included as
#'  model covariates.
#' @param ... Additional arguments to be passed to `ranger::ranger()`.
#'
#' @return A list with the following elements:
#'  - `tau_hat`: Predicted conditional average treatment effect for each observation
#'  - `variance_estimates`: Set to NULL when `aggregation_method` set to "ensembleforest.
#'  - `var_importance`: TBD
fit_ensemble_forest <- function(aug_data,
                                study_col,
                                covariate_col,
                                ...) {
  extra_args <- list(...)
  relevant_args <- extra_args[names(extra_args) %in% names(formals(ranger::ranger))]
  relevant_args[["importance"]] <- "impurity"
  relevant_args[["keep.inbag"]] <- TRUE

  fml <- as.formula(paste("tau_hat ~ ", study_col, " + ", paste(covariate_col, collapse="+")))

  fit <- do.call(ranger::ranger,
                 c(list(formula = fml,
                        data = aug_data),
                   relevant_args))

  var_import <- tibble::tibble(
    variable = names(fit$variable.importance),
    importance = as.numeric(fit$variable.importance)
  )

  return(list(tau_hat = fit$predictions[which(aug_data$S == aug_data$model_study)],
              variance_estimates = NULL,
              var_importance = var_import))
}
