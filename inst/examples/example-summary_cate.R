# inst/examples/example-summary_cate.R

cate_object <- estimate_cate(
  trial_tbl = dummy_tbl,
  estimation_method = "causalforest",
  aggregation_method = "studyindicator",
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response"
)

summary(cate_object)
