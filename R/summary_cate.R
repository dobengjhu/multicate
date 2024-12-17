summary.cate <- function(object) {
  assertthat::assert_that(
    "cate" %in% class(object),
    msg = "Object must be of class cate"
  )

  assertthat::assert_that(
    "estimation_method" %in% names(object),
    msg = "cate object must include an `estimation_method` element"
  )

  assertthat::assert_that(
    object$estimation_method %in% c("causalforest", "slearner"),
    msg = "Estimation method must be 'causalforest' or 'slearner'."
  )

  if (object$estimation_method == "causalforest") {
    ate <- grf::average_treatment_effect(object$estimation_object,
                                                      target.sample = "all")
  } else {
    sbart <- object$estimation_object
    w_fac <- ifelse(object$model[[object$treatment_col]] == 1, 1, -1)
    diffs <- sweep(sbart$yhat.train - sbart$yhat.test,
                   2,
                   w_fac,
                   '*')
    mean_diffs <- apply(diffs, 1, mean)
    ate <- c(mean(mean_diffs), sd(mean_diffs))
  }


}
