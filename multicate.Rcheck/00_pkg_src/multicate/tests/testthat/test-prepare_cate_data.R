dummy_named_args <- list(
  trial_tbl = dummy_tbl,
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response",
  covariate_col = paste0("var", 1:5)
)

dummy_named_args_si <- c(dummy_named_args,
                         list(estimation_method = "studyindicator"))
dummy_named_args_ef <- c(dummy_named_args,
                         list(estimation_method = "ensembleforest"))

expected_si_tbl <- dummy_tbl %>%
  dplyr::select(var1:var5) %>%
  dplyr::mutate(studyid_study1 = as.integer(rep(c(1,0,0), each = 500)),
                studyid_study2 = as.integer(rep(c(0,1,0), each = 500)),
                studyid_study3 = as.integer(rep(c(0,0,1), each = 500)))
expected_ef_tbl <- dummy_tbl %>%
  dplyr::select(var1:var5)
expected_tx_vec <- dummy_tbl %>%
  dplyr::pull(tx)
expected_outcome_vec <- dummy_tbl %>%
  dplyr::pull(response)
expected_study_vec <- dummy_tbl %>%
  dplyr::pull(studyid)
expected_study_col <- "studyid"

test_that("prepare_cate_data() returns correct result when estimation_method = 'studyindicator'", {
  result <- prepare_cate_data(dummy_named_args_si,
                              "studyindicator")

  expect_identical(result$feature_tbl, expected_si_tbl)
  expect_identical(result$treatment_vec, expected_tx_vec)
  expect_identical(result$outcome_vec, expected_outcome_vec)
  expect_identical(result$study_vec, expected_study_vec)
  expect_identical(result$study_col, expected_study_col)
})

test_that("prepare_cate_data() returns correct result when estimation_method = 'ensembleforest'", {
  result <- prepare_cate_data(dummy_named_args_si,
                              "ensembleforest")

  expect_identical(result$feature_tbl, expected_ef_tbl)
  expect_identical(result$treatment_vec, expected_tx_vec)
  expect_identical(result$outcome_vec, expected_outcome_vec)
  expect_identical(result$study_vec, expected_study_vec)
  expect_identical(result$study_col, expected_study_col)
})
