dummy_cate_object_sl <- estimate_cate(trial_tbl = dummy_tbl,
                                      estimation_method = "slearner",
                                      aggregation_method = "studyindicator",
                                      study_col = "studyid",
                                      treatment_col = "tx",
                                      outcome_col = "response",
                                      verbose = FALSE)

dummy_cate_object_cf <- estimate_cate(trial_tbl = dummy_tbl,
                                      estimation_method = "causalforest",
                                      aggregation_method = "studyindicator",
                                      study_col = "studyid",
                                      treatment_col = "tx",
                                      outcome_col = "response")

dummy_cate_object_rang <- estimate_cate(trial_tbl = dummy_tbl,
                                        estimation_method = "causalforest",
                                        aggregation_method = "ensembleforest",
                                        study_col = "studyid",
                                        treatment_col = "tx",
                                        outcome_col = "response",
                                        verbose = FALSE)

dummy_cate_object_ss <- estimate_cate(trial_tbl = dummy_tbl,
                                        estimation_method = "causalforest",
                                        aggregation_method = "studyspecific",
                                        study_col = "studyid",
                                        treatment_col = "tx",
                                        outcome_col = "response",
                                        verbose = FALSE)

test_that("summary.cate runs without errors", {
  expect_error(summary.cate(dummy_cate_object_cf), NA)
  expect_error(summary.cate(dummy_cate_object_sl), NA)
  expect_error(summary.cate(dummy_cate_object_rang), NA)
  expect_error(summary.cate(dummy_cate_object_ss), NA)
})

test_that("summary.cate generates expected errors/warnings", {
  expect_snapshot(
    plot.cate("Not a cate object"),
    error = TRUE
  )
})
