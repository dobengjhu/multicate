#' Title
#'
#' @param sbart
#' @param w
#'
#' @return
#'
#' @examples
estimate_sbart_tau <- function(yhat_observed, yhat_counterfactual, w) {
  mu1 <- (w * yhat_observed) + ((1 - w) * yhat_counterfactual)
  mu0 <- ((1 - w) * yhat_observed) + (w * yhat_counterfactual)

  mu1 - mu0
}
