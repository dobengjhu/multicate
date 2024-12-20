dummy_named_args <- list(
  trial_tbl = dummy_tbl,
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response",
  covariate_col = paste0("var", 1:5)
)

dummy_named_args_cf <- c(dummy_named_args,
                         list(estimation_method = "causalforest"))
dummy_named_args_sl <- c(dummy_named_args,
                         list(estimation_method = "slearner"))

test_that("generate_tau.studyindicator_args returns correct structure for causalforest", {
  result <- generate_tau.studyindicator_args(dummy_named_args_cf)

  expect_named(result, c("tau_hat", "variance_estimates", "var_importance", "fit_object"))

  expect_true(is.numeric(result$tau_hat))
  expect_equal(length(result$tau_hat), nrow(dummy_named_args_cf$trial_tbl))

  expect_true(is.numeric(result$variance_estimates))
  expect_equal(length(result$variance_estimates), nrow(dummy_named_args_cf$trial_tbl))

  expect_named(result$var_importance, c("variable", "importance"))
  expect_equal(nrow(result$var_importance), 8)

  expect_s3_class(result$fit_object, "causal_forest")
})

test_that("generate_tau.studyindicator_args returns correct structure for slearner", {
  result <- generate_tau.studyindicator_args(dummy_named_args_sl, verbose = FALSE)

  expect_named(result, c("tau_hat", "variance_estimates", "var_importance", "fit_object"))

  expect_true(is.numeric(result$tau_hat))
  expect_equal(length(result$tau_hat), nrow(dummy_named_args_cf$trial_tbl))

  expect_true(is.numeric(result$variance_estimates))
  expect_equal(length(result$variance_estimates), nrow(dummy_named_args_cf$trial_tbl))

  expect_named(result$var_importance, c("variable", "importance"))
  expect_equal(nrow(result$var_importance), 9)

  expect_s3_class(result$fit_object, "bart")
})
