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

test_that("plot_vteffect runs without errors", {
  expect_error(plot_vteffect(dummy_cate_object_sl, "var1"), NA)
  expect_error(plot_vteffect(dummy_cate_object_cf, "var1"), NA)
  expect_error(plot_vteffect(dummy_cate_object_rang, "var1"), NA)
})

test_that("plot_vteffect generates expected errors/warnings", {
  expect_snapshot(
    plot_vteffect("Not a cate object", "var1"),
    error = TRUE
  )

  expect_snapshot(
    plot_vteffect(dummy_cate_object_cf, 1234),
    error = TRUE
  )

  expect_snapshot(
    plot_vteffect(dummy_cate_object_cf, "foobar"),
    error = TRUE
  )
})
