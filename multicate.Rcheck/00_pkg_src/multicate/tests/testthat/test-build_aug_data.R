dummy_named_args <- list(
  trial_tbl = dummy_tbl,
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response",
  covariate_col = paste0("var", 1:5)
)

dummy_named_args_cf <- c(dummy_named_args,
                         list(estimation_method = "causalforest"))
class(dummy_named_args_cf) <- "causalforest_args"
dummy_named_args_sl <- c(dummy_named_args,
                         list(estimation_method = "slearner"))
class(dummy_named_args_sl) <- "slearner_args"

expected_nrow <- nrow(dummy_tbl) * 3
expected_study_val <- c("study1", "study2", "study3")

test_that("build_aug_data returns correct structure", {
  result_cf <- build_aug_data(dummy_named_args_cf)

  expect_equal(nrow(result_cf), expected_nrow)
  expect_equal(as.character(unique(result_cf$model_study)), expected_study_val)

  result_sl <- build_aug_data(dummy_named_args_sl)
  expect_equal(nrow(result_sl), expected_nrow)
  expect_equal(as.character(unique(result_sl$model_study)), expected_study_val)
})
