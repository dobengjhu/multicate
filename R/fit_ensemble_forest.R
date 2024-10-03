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
                                covarite_col,
                                ...) {
  extra_args <- list(...)
  relevant_args <- extra_args[names(extra_args) %in% names(formals(ranger::ranger))]

  fml <- as.formula(paste("tau_hat ~ ", site_col, " + ", paste(covarite_col, collapse="+")))

  fit <- do.call(ranger::ranger,
                 c(list(formula = fml, data = aug_data),
                   relevant_args))
  fit$predictions[which(aug_data$S == aug_data$model_site)]
}
