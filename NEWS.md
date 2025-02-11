# multicate 1.0.0

## First Public Release
This is the first official release of `multicate`! The package provides functions for performing 
machine learning methods that estimate theconditional average treatment effect (CATE) by combining 
data from multiple studies. Additionalfunctions can be used to estimate prediction intervals for the 
CATE in a target sample based on the aforementioned models.

### Features 
- Implements `estimate_cate()` which estimates conditional average treatment effect (CATE) from 
multiple studies.
- Supports custom summarization and visualizations with `summary.cate()` and `plot.cate()` (S3) and 
`plot_vteffect()`.
- Provides `predict.cate()` (S3) that generates prediction intervals for covariate profiles
in target dataset based on CATE model.
- Includes built-in dataset `dummy_tbl` for testing.
- Comprehensive documentation and examples included.

### Installation
Install from Github:
``` r
# install.packages("pak")
pak::pak("dobengjhu/multicate")
```
