#' Assert tbl Object Contains Specified Column(s)
#'
#' @param table_object tbl. A tbl in which to check for supplied column name(s).
#' @param ... One or more column names (as strings).
#'
#' @return Silent return unless validation fail.
assert_column_names_exist <- function(table_object, ...) {
  dots <- list(...)
  assertthat::assert_that(
    all(purrr::map_chr(dots, class) == "character"),
    msg = "At least one of the supplied values is not a string."
  )

  column_names <- unlist(dots)

  assertthat::assert_that(
    all(column_names %in% colnames(table_object)),
    msg = glue::glue(
      "At least one of the following columns does not exist in the supplied table object: \n
      {paste(column_names, collapse = ' | ')}"
    )
  )
}

#' Assert Column in tbl Object is of a Specified Class
#'
#' @param table_object tbl. A tbl containing the column to validate.
#' @param column_name string. The name of a column to be validated.
#' @param column_class character vector. A vector of classes against which to validate.
#'
#' @return Silent return unless validation fails.
assert_column_class <- function(table_object,
                                column_name,
                                column_class) {
  assertthat::assert_that(
    length(column_name) == 1,
    msg = "Column class validation only works on one column at a time."
  )

  assert_column_names_exist(table_object, column_name)

  column_class <- column_class %>%
    tolower()

  class_set <- table_object %>%
    purrr::map(.f = class)

  assertthat::assert_that(
    any(class_set[[column_name]] %in% column_class),
    msg = glue::glue(
      "{column_name} does not possess one of the following classes: {paste(column_class,
      collapse = ' | ')}"
    )
  )
}

#' Estimate CATE for S-learner Using BART
#'
#' @param observed matrix. An array/matrix of posterior samples from
#'  \link[dbarts:bart]{dbarts::bart()} using treatment indicators based on user-provided treatment
#'  assignments, `w`.
#' @param counterfactual matrix. An array/matrix of posterior samples from
#'  \link[dbarts:bart]{dbarts::bart()} using treatment indicators based on the inverse of
#'  user-provided treatment assignments, `w`.
#' @param w numeric vector. Vector of treatment indicators.
#'
#' @return A list with the following elements:
#'  - `means_cate`: Estimated conditional average treatment effects.
#'  - `vars_cate`: Variance associated with `means_cate`
estimate_sbart_tau <- function(observed,
                               counterfactual,
                               w) {
  yhat_observed <- apply(observed, 2, mean)
  yhat_counterfactual <- apply(counterfactual, 2, mean)
  vars_observed <- apply(observed, 2, var)
  vars_counterfactual <- apply(counterfactual, 2, var)

  mu1 <- (w * yhat_observed) + ((1 - w) * yhat_counterfactual)
  mu0 <- ((1 - w) * yhat_observed) + (w * yhat_counterfactual)

  list(means_cate = mu1 - mu0,
       vars_cate = vars_observed + vars_counterfactual)
}
