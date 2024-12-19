dummy_tbl <- tibble::tibble(
  studyid = c("study1", "study2"),
  var1 = c(1, 2),
  var2 = c(3, 4),
  tx = c(0, 1),
  response = c(100, 110)
)

test_that("assert_column_names_exist works correctly when given a single column name", {
  expect_silent(assert_column_names_exist(dummy_tbl, "studyid"))
  expect_snapshot(
    assert_column_names_exist(dummy_tbl, "foobar"),
    error = TRUE
  )
})

test_that("assert_column_names_exist works correctly when given more than one column names", {
  expect_silent(assert_column_names_exist(dummy_tbl, "studyid", "response"))
  expect_snapshot(
    assert_column_names_exist(dummy_tbl, "foobar", "response"),
    error = TRUE
  )
})
