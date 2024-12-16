dummy_tbl <- tibble::tribble(
  ~studyid,   ~tx, ~var1,   ~var2,   ~var3,   ~var4,   ~var5,   ~response,
  "study1", 0, -1.68,  1.66, -0.593,  2.37, -0.565,  3.00,
  "study1", 1, -1.42,  1.52,  0.177,  0.299,  1.56,  3.02,
  "study1", 0,  0.640,  0.190, -0.0838, 0.0324,  0.601,  0.894,
  "study1", 1, -0.727, -1.64, -0.938, -1.42,  0.769, -2.45,
  "study1", 1,  1.11,  1.76,  0.175, -1.12, -0.584,  4.14,
  "study2", 0, -1.91, -0.680, -0.918, -0.110, -0.826, -1.61,
  "study2", 0,  0.721,  0.263, -1.70,  0.133, -1.16, -1.27,
  "study2", 0, -0.633, -2.62, -2.20, -0.972, -0.403, -5.73,
  "study2", 1, -1.24,  0.976, -1.79, -0.453,  1.32,  0.628,
  "study2", 1,  1.58, -0.807, -0.174,  0.279,  0.0533, -0.356,
  "study3", 1,  0.705, -0.0402,  0.786,  0.799, -0.265,  4.80,
  "study3", 0, -0.952, -0.897,  2.36,  1.14,  1.48,  2.96,
  "study3", 1, -1.41,  0.169,  0.652,  1.44, -1.07,  4.17,
  "study3", 1, -1.24,  1.01, -0.298, -0.493, -0.890,  2.08,
  "study3", 1,  1.47, -0.669, -0.265,  0.908, -0.264,  3.79
)

dummy_named_args <- list(
  trial_tbl = dummy_tbl,
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response",
  covariate_col = paste0("var", 1:5)
)
dummy_named_args_si <- c(dummy_named_args, list(estimation_method = "studyindicator"))
dummy_named_args_ef <- c(dummy_named_args, list(estimation_method = "ensembleforest"))

test_that("prepare_cate_data() returns correct result when estimation_method = 'studyindicator'", {
  result <- prepare_cate_data(dummy_named_args_si,
                              "studyindicator")
})
