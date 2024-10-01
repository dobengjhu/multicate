#' Title
#'
#' @param named_args
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
build_aug_data <- function(named_args,
                           ...) {
  site_id <- named_args$trial_tbl %>%
    dplyr::distinct(!!rlang::sym(named_args[["site_col"]])) %>%
    dplyr::pull()
  dots <- list(...)

  purrr::map(.x = site_id,
             .f = function(x) do.call(predict_tau, c(list(named_args, x), dots))
  ) %>%
    purrr::reduce(dplyr::bind_rows)
}
