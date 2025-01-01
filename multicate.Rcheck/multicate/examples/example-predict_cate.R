# inst/examples/example-predict_cate.R

new_dummy_tbl <- tibble::tribble(
  ~tx, ~var1,   ~var2,   ~var3,    ~var4,   ~var5,   ~response,
  0,  -0.728,   1.66,   -0.213,   -0.0621,  0.252,   1.26,
  0,  -0.464,   0.230,   0.579,   -1.17,    0.0513, -0.235,
  0,   1.52,   -0.792,   0.951,    0.869,  -0.312,   1.42,
  0,   0.410,  -0.945,  -1.49,    -1.87,   -1.35,   -3.10,
  1,  -0.223,   2.20,    0.0242,   0.419,   0.333,   3.56,
  1,  -0.187,  -0.439,  -0.557,   -0.0679,  0.497,   0.0518,
  1,   1.41,   -0.830,  -0.306,    1.12,    0.648,   2.61,
  1,  -0.437,   1.22,    1.18,    -1.63,   -1.09,    0.833,
)

cate_object <- estimate_cate(
  trial_tbl = dummy_tbl,
  estimation_method = "causalforest",
  aggregation_method = "studyindicator",
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response"
)

predict(cate_object, new_dummy_tbl)
