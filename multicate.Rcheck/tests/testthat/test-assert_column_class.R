dummy_tbl <- tibble::tibble(
  studyid = c("study1", "study2"),
  tx = c(1L, 0L),
  var1 = factor(10001, 10002),
  response = c(110, 112)
)

test_that("assert_column_class works with both sing and multiple classes", {
  expect_silent(assert_column_class(dummy_tbl, "studyid", "character"))
  expect_silent(assert_column_class(dummy_tbl, "tx", "INTEGER"))
  expect_silent(assert_column_class(dummy_tbl, "var1", "FActOr"))
  expect_silent(assert_column_class(dummy_tbl, "response", "numeric"))
  expect_silent(assert_column_class(dummy_tbl, "response", c("numeric", "character")))
  expect_snapshot(
    assert_column_class(dummy_tbl, c("studyid", "tx"), c("character", "integer")),
    error = TRUE
  )
  expect_snapshot(
    assert_column_class(dummy_tbl, "studyid", "numeric"),
    error = TRUE
  )
})
