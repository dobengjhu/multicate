#' Build Augmented Dataset for Ensemble Forest
#'
#' @param named_args list. A list of pre-specified argument values from
#'  `multicate::estimate_cate()`.
#' @param ... list. Arguments to be passed to `estimation_method` and/or `aggregation_method`.
build_aug_data <- function(named_args,
                           ...) {
  study_id <- named_args$trial_tbl %>%
    dplyr::distinct(!!rlang::sym(named_args[["study_col"]])) %>%
    dplyr::pull()
  dots <- list(...)

  purrr::map(.x = study_id,
             .f = function(x) do.call(generate_aug_tau, c(list(named_args, x), dots))
  ) %>%
    purrr::reduce(dplyr::bind_rows)
}
