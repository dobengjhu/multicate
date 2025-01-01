#' Augmented Dummy Dataset for Unit Testing
#'
#' @description A dataset containing simulated study data for unit testing
#' ensemble forest-related functions in the package.
#'
#' @format A tibble with 4500 rows and 10 variables:
#' \describe{
#'   \item{studyid}{Study ID, a character string.}
#'   \item{tx}{Treatment assignment, 0 for control and 1 for treated.}
#'   \item{var1}{Covariate 1, a numeric variable.}
#'   \item{var2}{Covariate 2, a numeric variable.}
#'   \item{var3}{Covariate 3, a numeric variable.}
#'   \item{var4}{Covariate 3, a numeric variable.}
#'   \item{var5}{Covariate 3, a numeric variable.}
#'   \item{response}{Outcome variable, a numeric variable.}
#'   \item{tau_hat}{Estimated CATE, a numeric variable.}
#'   \item{model_study}{Study ID for model used to generate tau hat, a character string.}
#' }
#'
#' @examples
#' aug_dummy_tbl
"aug_dummy_tbl"
