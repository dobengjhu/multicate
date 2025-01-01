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

test_that("plot.cate runs without errors", {
  expect_error(plot.cate(dummy_cate_object_cf, ask = FALSE), NA)

  suppressWarnings(
    expect_error(plot.cate(dummy_cate_object_sl, ask = FALSE), NA)
  )

  suppressWarnings(
    expect_error(plot.cate(dummy_cate_object_rang, ask = FALSE), NA)
  )
})

test_that("plot.cate generates expected errors/warnings", {
  expect_snapshot(
    plot.cate("Not a cate object"),
    error = TRUE
  )

  expect_snapshot(
    plot.cate(dummy_cate_object_sl, which_plot = 10),
    error = TRUE
  )

  expect_snapshot(
    plot.cate(dummy_cate_object_sl, which_plot = "3"),
    error = TRUE
  )

  expect_warning(
    plot.cate(dummy_cate_object_sl, which_plot = 4),
    "Object of class 'causal_forest' required for best linear projection figure."
  )

  expect_warning(
    plot.cate(dummy_cate_object_rang, which_plot = 4),
    "Object of class 'causal_forest' required for best linear projection figure."
  )

  expect_warning(
    plot.cate(dummy_cate_object_rang, which_plot = 3),
    "Variance estimates missing from model output. 95% CI plot will not be produced."
  )
})
