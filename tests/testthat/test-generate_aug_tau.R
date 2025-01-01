dummy_named_args <- list(
  trial_tbl = dummy_tbl,
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response",
  covariate_col = paste0("var", 1:5)
)

dummy_named_args_cf <- c(dummy_named_args,
                         list(estimation_method = "causalforest_args"))
dummy_named_args_sl <- c(dummy_named_args,
                         list(estimation_method = "slearner"))

test_that("generate_aug_tau.causalforest_args returns correct structure", {
  result <- generate_aug_tau.causalforest_args(dummy_named_args, study_val = "study1")

  expect_true(all(c("tau_hat", "model_study") %in% colnames(result)))
  expect_identical(dummy_tbl, result %>% dplyr::select(dplyr::all_of(colnames(dummy_tbl))))
  expect_true(is.numeric(result$tau_hat))
  expect_true(any(class(result) %in% c("tbl", "data.frame")))
})

test_that("generate_aug_tau.slearner_args returns correct structure", {
  result <- generate_aug_tau.slearner_args(dummy_named_args, study_val = "study1")

  expect_true(all(c("tau_hat", "model_study") %in% colnames(result)))
  expect_identical(dummy_tbl, result %>% dplyr::select(dplyr::all_of(colnames(dummy_tbl))))
  expect_true(is.numeric(result$tau_hat))
  expect_true(any(class(result) %in% c("tbl", "data.frame")))
})
