#' Estimate CATE for S-learner Using BART
#'
#' @param observed matrix. An array/matrix of posterior samples from
#'  \link[dbarts:bart]{dbarts::bart()} using treatment indicators based on user-provided treatment
#'  assignments, `w`.
#' @param counterfactual matrix. An array/matrix of posterior samples from
#'  \link[dbarts:bart]{dbarts::bart()} using treatment indicators based on the inverse of
#'  user-provided treatment assignments, `w`.
#' @param w numeric vector. Vector of treatment indicators.
#'
#' @return A list with the following elements:
#'  - `means_cate`: Estimated conditional average treatment effects.
#'  - `vars_cate`: Variance associated with `means_cate`
estimate_sbart_tau <- function(observed,
                               counterfactual,
                               w) {
  yhat_observed <- apply(observed, 2, mean)
  yhat_counterfactual <- apply(counterfactual, 2, mean)
  vars_observed <- apply(observed, 2, var)
  vars_counterfactual <- apply(counterfactual, 2, var)

  mu1 <- (w * yhat_observed) + ((1 - w) * yhat_counterfactual)
  mu0 <- ((1 - w) * yhat_observed) + (w * yhat_counterfactual)

  list(means_cate = mu1 - mu0,
       vars_cate = vars_observed + vars_counterfactual)
}
