# inst/examples/example-estimate_cate.R

estimate_cate(
  trial_tbl = dummy_tbl,
  estimation_method = "causalforest",
  aggregation_method = "studyindicator",
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response"
)

estimate_cate(
  trial_tbl = dummy_tbl,
  estimation_method = "slearner",
  aggregation_method = "ensembleforest",
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response"
)
