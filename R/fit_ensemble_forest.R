#' Title
#'
#' @param aug_data
#' @param site_col
#' @param covarite_col
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fit_ensemble_forest <- function(aug_data,
                                site_col,
                                covariate_col,
                                ...) {
  extra_args <- list(...)
  extra_args[["importance"]] <- "impurity"
  relevant_args <- extra_args[names(extra_args) %in% names(formals(ranger::ranger))]

  fml <- as.formula(paste("tau_hat ~ ", site_col, " + ", paste(covariate_col, collapse="+")))

  fit <- do.call(ranger::ranger,
                 c(list(formula = fml, data = aug_data),
                   relevant_args))

  var_import <- tibble::tibble(
    variable = names(fit$variable.importance),
    importance = as.numeric(fit$variable.importance)
  )

  return(list(tau_hat = fit$predictions[which(aug_data$S == aug_data$model_site)],
              var_importance = var_import))
}
