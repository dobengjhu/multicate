dummy_tbl_extra_var <- dummy_tbl %>%
  dplyr::mutate(other = "foobar")

dummy_tbl_study_na <- dummy_tbl %>%
  dplyr::mutate(studyid = ifelse(dplyr::row_number() == 10, NA, studyid))

dummy_tbl_treatment_na <- dummy_tbl %>%
  dplyr::mutate(tx = ifelse(dplyr::row_number() == 10, NA, tx))

dummy_tbl_outcome_na <- dummy_tbl %>%
  dplyr::mutate(response = ifelse(dplyr::row_number() == 10, NA, response))

dummy_tbl_covariate_na <- dummy_tbl %>%
  dplyr::mutate(var4 = ifelse(dplyr::row_number() == 10, NA, var4))

expected_object_names <- c("estimation_method",
                           "aggregation_method",
                           "model",
                           "var_importance",
                           "study_col",
                           "treatment_col",
                           "outcome_col",
                           "covariate_col",
                           "extra_args",
                           "estimation_object"
)

original_colnames <- colnames(dummy_tbl)
original_colnames_extra_var <- colnames(dummy_tbl_extra_var)
covariate_colnames <- paste0("var", 1:5)

test_that("estimate_cate returns correct structure with valid inputs (causalforest / studyindicator)", {
  result <- estimate_cate(
    trial_tbl = dummy_tbl,
    estimation_method = "causalforest",
    aggregation_method = "studyindicator",
    study_col = "studyid",
    treatment_col = "tx",
    outcome_col = "response"
  )

  expect_s3_class(result, "cate")
  expect_named(result, expected_object_names)
  expect_identical(result$estimation_method, "causalforest")
  expect_identical(result$aggregation_method, "studyindicator")
  expect_identical(result$study_col, "studyid")
  expect_identical(result$treatment_col, "tx")
  expect_identical(result$outcome_col, "response")
  expect_identical(result$covariate_col, covariate_colnames)
  expect_true(all(c(original_colnames, "tau_hat", "variance_estimates") %in% colnames(result$model)))

  result_extra_var <- estimate_cate(
    trial_tbl = dummy_tbl_extra_var,
    estimation_method = "causalforest",
    aggregation_method = "studyindicator",
    study_col = "studyid",
    treatment_col = "tx",
    outcome_col = "response",
    drop_col = "other"
  )

  expect_identical(result_extra_var$covariate_col, covariate_colnames)
  expect_true(all(c(original_colnames_extra_var, "tau_hat", "variance_estimates") %in% colnames(result_extra_var$model)))

  result_subset_var <- estimate_cate(
    trial_tbl = dummy_tbl_extra_var,
    estimation_method = "causalforest",
    aggregation_method = "studyindicator",
    study_col = "studyid",
    treatment_col = "tx",
    outcome_col = "response",
    covariate_col = c("var1", "var2", "var3", "var4", "var5")
  )

  expect_identical(result_subset_var$covariate_col, covariate_colnames)
  expect_true(all(c(original_colnames_extra_var, "tau_hat", "variance_estimates") %in% colnames(result_subset_var$model)))

  result_extra_arg <- estimate_cate(
    trial_tbl = dummy_tbl,
    estimation_method = "causalforest",
    aggregation_method = "studyindicator",
    study_col = "studyid",
    treatment_col = "tx",
    outcome_col = "response",
    num.trees = 1234,
    foo = "bar"
  )

  expect_identical(result_extra_arg$extra_args, list(num.trees = 1234, foo = "bar"))
  expect_true(result_extra_arg$estimation_object[["_num_trees"]] == 1234)
})

test_that("estimate_cate returns correct structure with valid inputs (causalforest / ensembleforest)", {
  result <- estimate_cate(
    trial_tbl = dummy_tbl,
    estimation_method = "causalforest",
    aggregation_method = "ensembleforest",
    study_col = "studyid",
    treatment_col = "tx",
    outcome_col = "response"
  )

  expect_s3_class(result, "cate")
  expect_named(result, expected_object_names)
  expect_identical(result$estimation_method, "causalforest")
  expect_identical(result$aggregation_method, "ensembleforest")
  expect_identical(result$study_col, "studyid")
  expect_identical(result$treatment_col, "tx")
  expect_identical(result$outcome_col, "response")
  expect_identical(result$covariate_col, covariate_colnames)
  expect_true("tau_hat" %in% colnames(result$model))
})

test_that("estimate_cate returns correct structure with valid inputs (causalforest / studyspecific)", {
  result <- estimate_cate(
    trial_tbl = dummy_tbl,
    estimation_method = "causalforest",
    aggregation_method = "studyspecific",
    study_col = "studyid",
    treatment_col = "tx",
    outcome_col = "response"
  )

  expect_s3_class(result, "cate")
  expect_named(result, expected_object_names)
  expect_identical(result$estimation_method, "causalforest")
  expect_identical(result$aggregation_method, "studyspecific")
  expect_identical(result$study_col, "studyid")
  expect_identical(result$treatment_col, "tx")
  expect_identical(result$outcome_col, "response")
  expect_identical(result$covariate_col, covariate_colnames)
  expect_true("tau_hat" %in% colnames(result$model))
  expect_identical(class(result$var_importance), "list")
  expect_true(length(result$var_importance) == 3)
  expect_identical(class(result$estimation_object), "list")
  expect_true(length(result$estimation_object) == 3)
})

test_that("estimate_cate returns correct structure with valid inputs (slearner / studyindicator)", {
  result <- estimate_cate(
    trial_tbl = dummy_tbl,
    estimation_method = "slearner",
    aggregation_method = "studyindicator",
    study_col = "studyid",
    treatment_col = "tx",
    outcome_col = "response",
    verbose = FALSE
  )

  expect_s3_class(result, "cate")
  expect_named(result, expected_object_names)
  expect_identical(result$estimation_method, "slearner")
  expect_identical(result$aggregation_method, "studyindicator")
  expect_identical(result$study_col, "studyid")
  expect_identical(result$treatment_col, "tx")
  expect_identical(result$outcome_col, "response")
  expect_identical(result$covariate_col, covariate_colnames)
  expect_true(all(c(original_colnames, "tau_hat", "variance_estimates") %in% colnames(result$model)))
})

test_that("estimate_cate returns correct structure with valid inputs (slearner / ensembleforest)", {
  result <- estimate_cate(
    trial_tbl = dummy_tbl,
    estimation_method = "slearner",
    aggregation_method = "ensembleforest",
    study_col = "studyid",
    treatment_col = "tx",
    outcome_col = "response"
  )

  expect_s3_class(result, "cate")
  expect_named(result, expected_object_names)
  expect_identical(result$estimation_method, "slearner")
  expect_identical(result$aggregation_method, "ensembleforest")
  expect_identical(result$study_col, "studyid")
  expect_identical(result$treatment_col, "tx")
  expect_identical(result$outcome_col, "response")
  expect_identical(result$covariate_col, covariate_colnames)
  expect_true("tau_hat" %in% colnames(result$model))
})

test_that("estimate_cate returns correct structure with valid inputs (slearner / studyspecific)", {
  result <- estimate_cate(
    trial_tbl = dummy_tbl,
    estimation_method = "slearner",
    aggregation_method = "studyspecific",
    study_col = "studyid",
    treatment_col = "tx",
    outcome_col = "response"
  )

  expect_s3_class(result, "cate")
  expect_named(result, expected_object_names)
  expect_identical(result$estimation_method, "slearner")
  expect_identical(result$aggregation_method, "studyspecific")
  expect_identical(result$study_col, "studyid")
  expect_identical(result$treatment_col, "tx")
  expect_identical(result$outcome_col, "response")
  expect_identical(result$covariate_col, covariate_colnames)
  expect_true("tau_hat" %in% colnames(result$model))
  expect_identical(class(result$var_importance), "list")
  expect_true(length(result$var_importance) == 3)
  expect_identical(class(result$estimation_object), "list")
  expect_true(length(result$estimation_object) == 3)
})

test_that("estimate_cate raises error for invalid variable values", {
  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl %>% dplyr::mutate(tx = paste0("Treatment ", tx)),
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response"
    ),
    error = TRUE
  )

  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl %>% dplyr::mutate(tx = tx + 1),
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response"
    ),
    error = TRUE
  )

  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl %>% dplyr::mutate(response = as.character(response)),
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response"
    ),
    error = TRUE
  )
})

test_that("estimate_cate raises error for missing columns", {
  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl %>% dplyr::select(-studyid),
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response"
    ),
    error = TRUE
  )

  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl %>% dplyr::select(-tx),
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response"
    ),
    error = TRUE
  )

  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl %>% dplyr::select(-response),
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response"
    ),
    error = TRUE
  )

  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl,
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response",
      covariate_col = "foobar"
    ),
    error = TRUE
  )

  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl,
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response",
      covariate_col = "foobar"
    ),
    error = TRUE
  )

  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl_study_na,
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response"
    ),
    error = TRUE
  )

  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl_treatment_na,
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response"
    ),
    error = TRUE
  )

  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl_outcome_na,
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response"
    ),
    error = TRUE
  )

  expect_snapshot(
    estimate_cate(
      trial_tbl = dummy_tbl_covariate_na,
      estimation_method = "causalforest",
      aggregation_method = "studyindicator",
      study_col = "studyid",
      treatment_col = "tx",
      outcome_col = "response"
    ),
    error = TRUE
  )
})
