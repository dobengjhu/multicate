#' Title
#'
#' @param arg
#' @param aggregation_method
#'
#' @return
#' @export
#'
#' @examples
prepare_cate_data <- function(arg,
                              aggregation_method) {
  # prep feature tbl
  treatment_vec <- arg[["trial_tbl"]][[arg[["treatment_col"]]]]
  outcome_vec <- arg[["trial_tbl"]][[arg[["outcome_col"]]]]
  site_vec <- arg[["trial_tbl"]][[arg[["site_col"]]]]
  feature_tbl <- arg[["trial_tbl"]] %>%
    dplyr::select(!!rlang::sym(arg[["site_col"]]), dplyr::all_of(arg[["covariate_col"]])) %>%
    {
      if (aggregation_method == "studyindicator") {
        fastDummies::dummy_cols(.,
                                select_columns = arg[["site_col"]],
                                remove_selected_columns = TRUE)
      } else if (aggregation_method == "ensembleforest") {
        dplyr::select(.,
                      -!!rlang::sym(arg[["site_col"]]))
      }
    }

  list(feature_tbl = feature_tbl,
       treatment_vec = treatment_vec,
       outcome_vec = outcome_vec,
       site_vec = site_vec,
       site_col = site_col)
}
