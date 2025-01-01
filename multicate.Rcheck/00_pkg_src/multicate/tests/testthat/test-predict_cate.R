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

dummy_cate_object_cf_studies <- estimate_cate(trial_tbl = dummy_tbl %>%
                                                dplyr::filter(studyid %in% c("study1", "study2")),
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

new_dummy_tbl <- tibble::tribble(
  ~tx, ~var1,   ~var2,   ~var3,    ~var4,   ~var5,   ~response,
  0,  -0.728,   1.66,   -0.213,   -0.0621,  0.252,   1.26,
  0,  -0.464,   0.230,   0.579,   -1.17,    0.0513, -0.235,
  0,   1.52,   -0.792,   0.951,    0.869,  -0.312,   1.42,
  0,   0.410,  -0.945,  -1.49,    -1.87,   -1.35,   -3.10,
  0,   0.0947, -0.743,   0.433,   -0.479,  -0.602,  -0.356,
  0,   0.172,   2.12,    1.20,     0.994,   0.513,   4.45,
  0,  -1.41,    1.13,   -0.00402, -0.853,  -0.520,   0.588,
  0,  -0.690,  -0.973,   0.419,   -0.741,  -0.0238, -1.23,
  0,  -0.273,   0.0296,  1.54,    -0.0201,  0.339,   1.58,
  0,  -0.427,   0.210,  -1.07,     2.41,   -1.22,    1.53,
  1,  -0.223,   2.20,    0.0242,   0.419,   0.333,   3.56,
  1,  -0.187,  -0.439,  -0.557,   -0.0679,  0.497,   0.0518,
  1,   1.41,   -0.830,  -0.306,    1.12,    0.648,   2.61,
  1,  -0.437,   1.22,    1.18,    -1.63,   -1.09,    0.833,
  1,  -0.0527,  0.981,  -0.988,   -1.01,    1.01,   -0.129,
  1,  -0.808,  -0.141,   1.14,     1.09,   -0.101,   1.73,
  1,   0.794,   0.795,  -1.23,    -0.591,  -0.400,   0.800,
  1,  -1.59,    1.33,    0.860,   -1.63,    0.527,   1.93,
  1,  -1.54,   -1.31,    0.350,   -0.411,   0.324,  -1.16,
  1,  -0.0693,  0.837,  -1.02,     0.0276,  0.231,   3.38
) %>%
  dplyr::mutate(other = "foobar")

expected_tbl_names <- c(colnames(new_dummy_tbl),
                        "tau_predicted",
                        "ci_lower",
                        "ci_upper"
)

test_that("predict.cate returns correct tbl structure with valid inputs", {
  result_cf <- predict.cate(
    dummy_cate_object_cf,
    new_dummy_tbl
  )

  expect_s3_class(result_cf, "tbl")
  expect_true(all(expected_tbl_names %in% colnames(result_cf)))

  result_sl <- predict.cate(
    dummy_cate_object_sl,
    new_dummy_tbl
  )

  expect_s3_class(result_sl, "tbl")
  expect_true(all(expected_tbl_names %in% colnames(result_sl)))

  result_sl_alpha <- predict.cate(
    dummy_cate_object_sl,
    new_dummy_tbl,
    alpha = 0.1
  )

  expect_s3_class(result_sl, "tbl")
  expect_true(all(expected_tbl_names %in% colnames(result_sl)))
  expect_true(all(result_sl$ci_lower < result_sl_alpha$ci_lower))
  expect_true(all(result_sl$ci_upper > result_sl_alpha$ci_upper))
})

test_that("predict.cate raises error for invalid treatment and/or response values", {
  expect_snapshot(
    predict.cate(
      "Not a cate object",
      new_dummy_tbl
    ),
    error = TRUE
  )

  expect_snapshot(
    predict.cate(
      dummy_cate_object_cf,
      "Not a tbl"
    ),
    error = TRUE
  )

  expect_snapshot(
    predict.cate(
      dummy_cate_object_rang,
      new_dummy_tbl
    ),
    error = TRUE
  )

  expect_snapshot(
    predict.cate(
      dummy_cate_object_cf,
      new_dummy_tbl,
      alpha = "not a number between 0 and 1"
    ),
    error = TRUE
  )

  expect_snapshot(
    predict.cate(
      dummy_cate_object_cf,
      new_dummy_tbl,
      alpha = 999
    ),
    error = TRUE
  )

  expect_snapshot(
    predict.cate(
      dummy_cate_object_cf,
      new_dummy_tbl,
      alpha = -0.05
    ),
    error = TRUE
  )

  expect_snapshot(
    predict.cate(
      dummy_cate_object_cf,
      new_dummy_tbl %>% dplyr::select(-tx)
    ),
    error = TRUE
  )

  expect_snapshot(
    predict.cate(
      dummy_cate_object_cf,
      new_dummy_tbl %>% dplyr::select(-var1)
    ),
    error = TRUE
  )

  expect_snapshot(
    predict.cate(
      dummy_cate_object_cf_studies,
      new_dummy_tbl
    ),
    error = TRUE
  )
})
