#' Create Feature Table for CATE Estimation
#'
#' @description
#' If `estimation_method` is "studyindicator", creates feature table with pre-specified covariates
#' and adds set of dummy variables for each study to `trial_tbl`. If `estimation_method` is
#' "ensembleforest", creates feature table with only pre-specified covariates (which will include
#' removing study identifier column).
#'
#' @param named_args list. A list of pre-specified argument values from \link{estimate_cate}.
#' @param aggregation_method string. Method for aggregating results across studies.
#'
#' @return A list with the following elements:
#'  - `feature tbl`: tbl with columns corresponding to pre-specified covariates
#'  - `treatment_vec`: vector of treatment indicators for each observation
#'  - `outcome_vec`: vector of outcome values for each observation
#'  - `study_vec`: vector of study IDs for each observation
#'  - `study_col`: name of study ID column
prepare_cate_data <- function(named_args,
                              aggregation_method) {

  treatment_vec <- named_args$trial_tbl[[named_args[["treatment_col"]]]]
  outcome_vec <- named_args$trial_tbl[[named_args[["outcome_col"]]]]
  study_vec <- named_args$trial_tbl[[named_args[["study_col"]]]]
  feature_tbl <- named_args$trial_tbl %>%
    dplyr::select(!!rlang::sym(named_args$study_col), dplyr::all_of(named_args$covariate_col)) %>%
    {
      if (aggregation_method == "studyindicator") {
        fastDummies::dummy_cols(.,
                                select_columns = named_args[["study_col"]],
                                remove_selected_columns = TRUE)
      } else if (aggregation_method == "ensembleforest") {
        dplyr::select(.,
                      -!!rlang::sym(named_args$study_col))
      }
    }

  list(feature_tbl = feature_tbl,
       treatment_vec = treatment_vec,
       outcome_vec = outcome_vec,
       study_vec = study_vec,
       study_col = named_args$study_col)
}
