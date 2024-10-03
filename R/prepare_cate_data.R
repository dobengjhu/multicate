#' Title
#'
#' @param args
#' @param aggregation_method
#'
#' @return
#' @export
#'
#' @examples
prepare_cate_data <- function(args,
                              aggregation_method) {
  # prep feature tbl
  treatment_vec <- args$trial_tbl[[args[["treatment_col"]]]]
  outcome_vec <- args$trial_tbl[[args[["outcome_col"]]]]
  site_vec <- args$trial_tbl[[args[["site_col"]]]]
  feature_tbl <- args$trial_tbl %>%
    dplyr::select(!!rlang::sym(args$site_col), dplyr::all_of(args$covariate_col)) %>%
    {
      if (aggregation_method == "studyindicator") {
        fastDummies::dummy_cols(.,
                                select_columns = args[["site_col"]],
                                remove_selected_columns = TRUE)
      } else if (aggregation_method == "ensembleforest") {
        dplyr::select(.,
                      -!!rlang::sym(args$site_col))
      }
    }

  list(feature_tbl = feature_tbl,
       treatment_vec = treatment_vec,
       outcome_vec = outcome_vec,
       site_vec = site_vec,
       site_col = args$site_col)
}
