# Mock input data
dummy_observed <- matrix(c(10.5, 7.6, 10.8,
                           11.7, 8.5, 9.4,
                           7.9, 9.4, 7.1,
                           14.2, 9.3, 9.2,
                           9.5, 9.3, 10.4),
                         ncol = 3,
                         byrow = TRUE)

dummy_counterfactual <- matrix(c(8.1, 10.4, 7.7,
                                 10.3, 7.8, 7.2,
                                 5.3, 6.5, 7.0,
                                 6.5, 6.2, 4.8,
                                 12.7, 8.3, 10.6),
                               ncol = 3,
                               byrow = TRUE)

w <- c(1, 0, 1)

expected_means_cate <- c(2.18, 0.98, 1.92)
expected_vars_cate <- c(14.45, 3.41, 6.39)

test_that("estimate_sbart_tau computes correct CATE estimates and variances", {
  result <- estimate_sbart_tau(dummy_observed, dummy_counterfactual, w)

  expect_equal(result$means_cate, expected_means_cate)
  expect_equal(result$vars_cate, expected_vars_cate)
})
