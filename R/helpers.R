#' Title
#'
#' @param sbart
#' @param w
#'
#' @return
#'
#' @examples
estimate_sbart_tau <- function(observed, counterfactual, w) {
  yhat_observed <- apply(observed, 2, mean)
  yhat_counterfactual <- apply(counterfactual, 2, mean)
  vars_observed <- apply(observed, 2, var)
  vars_counterfactual <- apply(counterfactual, 2, var)

  mu1 <- (w * yhat_observed) + ((1 - w) * yhat_counterfactual)
  mu0 <- ((1 - w) * yhat_observed) + (w * yhat_counterfactual)

  list(means_cate = mu1 - mu0,
       vars_cate = vars_observed + vars_counterfactual)
}
