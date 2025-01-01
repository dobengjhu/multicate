# inst/examples/example-plot_cate.R

cate_object <- estimate_cate(
  trial_tbl = dummy_tbl,
  estimation_method = "causalforest",
  aggregation_method = "studyindicator",
  study_col = "studyid",
  treatment_col = "tx",
  outcome_col = "response"
)

plot(cate_object)

## Use `which_plot` to select specific plots to produce
## Use `ask = FALSE` to automatically generate all plots
plot(cate_object,
     which_plot = c(1, 3, 5),
     ask = FALSE)
