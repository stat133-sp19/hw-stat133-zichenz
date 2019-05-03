context("Tests for private auxiliary functions")

test_that("mean works as expected", {
  expect_equal(aux_mean(6, 0.5), 3)
  expect_length(aux_mean(6, 0.5), 1)
  expect_type(aux_mean(6, 0.5), "double")
})

test_that("variance works as expected", {
  expect_equal(aux_variance(6, 0.5), 1.5)
  expect_length(aux_variance(6, 0.5), 1)
  expect_type(aux_variance(6, 0.5), "double")
})

test_that("mode works as expected", {
  expect_equal(aux_mode(6, 0.5), 3)
  expect_length(aux_mode(7, 0.5), 1)
  expect_type(aux_mode(8, 0.5), "integer")
})

test_that("skewness works as expected", {
  expect_equal(aux_skewness(6, 0.5), 0)
  expect_length(aux_skewness(7, 0.4), 1)
  expect_type(aux_skewness(8, 0.8), "double")
})

test_that("kurtosis works as expected", {
  expect_equal(aux_kurtosis(4, 0.5), -0.5)
  expect_length(aux_kurtosis(6, 0.5), 1)
  expect_type(aux_kurtosis(6, 0.5), "double")
})
