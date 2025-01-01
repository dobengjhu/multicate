test_that("fit_ensemble_forest returns correct structure", {
  result <- fit_ensemble_forest(
    aug_data = aug_dummy_tbl,
    study_col = "studyid",
    covariate_col = c("var1", "var2", "var3", "var4", "var5")
  )

  expect_named(result, c("tau_hat", "variance_estimates", "var_importance", "fit_object"))

  expect_true(is.numeric(result$tau_hat))
  expect_equal(length(result$tau_hat), 1500)

  expect_true(is.null(result$variance_estimates))
  expect_message(
    fit_ensemble_forest(
      aug_data = aug_dummy_tbl,
      study_col = "studyid",
      covariate_col = c("var1", "var2", "var3", "var4", "var5")
    ),
    "Variance not estimated when `aggregation_method` is 'ensembleforest'."
  )

  expect_named(result$var_importance, c("variable", "importance"))
  expect_equal(nrow(result$var_importance), 6)

  expect_s3_class(result$fit_object, "ranger")
})
