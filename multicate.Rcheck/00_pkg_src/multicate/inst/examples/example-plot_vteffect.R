# inst/examples/example-plot_vteffect.R

cate_object <- estimate_cate(
  trial_tbl = dummy_tbl,
  estimation_method = "causalforest",
  aggregation_method = "studyindicator",
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response"
)

plot_vteffect(cate_object, "var1")
plot_vteffect(cate_object, "var3")
