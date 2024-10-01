fit_ensemble_forest <- function(aug_data,
                                site_col,
                                covarite_col,
                                ...) {
  browser()
  fml <- as.formula(paste("tau_hat ~ ", site_col, " + ", paste(covarite_col, collapse="+")))

  fit <- ranger::ranger(formula = fml,
                        data = aug_data,
                        ...)
  fit$predictions[which(aug_data$S == aug_data$model_site)]
}
