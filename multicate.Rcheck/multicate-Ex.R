pkgname <- "multicate"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "multicate-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('multicate')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("aug_dummy_tbl")
### * aug_dummy_tbl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aug_dummy_tbl
### Title: Augmented Dummy Dataset for Unit Testing
### Aliases: aug_dummy_tbl
### Keywords: datasets

### ** Examples

aug_dummy_tbl



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aug_dummy_tbl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dummy_tbl")
### * dummy_tbl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dummy_tbl
### Title: Dummy Dataset for Unit Testing
### Aliases: dummy_tbl
### Keywords: datasets

### ** Examples

dummy_tbl



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dummy_tbl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("estimate_cate")
### * estimate_cate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: estimate_cate
### Title: Estimate Heterogeneous Treatment Effects Across Multiple Studies
### Aliases: estimate_cate

### ** Examples

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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("estimate_cate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.cate")
### * plot.cate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.cate
### Title: Plot Diagnostics for a CATE Object
### Aliases: plot.cate

### ** Examples

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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.cate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_vteffect")
### * plot_vteffect

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_vteffect
### Title: Plot Variable Treatment Effect
### Aliases: plot_vteffect

### ** Examples

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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_vteffect", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict.cate")
### * predict.cate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict.cate
### Title: Predict Heterogeneous Treatment Effects Based on Estimated CATEs
### Aliases: predict.cate

### ** Examples

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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict.cate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
