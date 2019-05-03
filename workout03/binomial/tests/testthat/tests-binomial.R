context("tests for main functions")

test_that("binomial choose works as expected", {
  expect_error(bin_choose(3, 4))
  expect_error(bin_choose(3, 2:8))
  expect_equal(bin_choose(4, 2), 6)
  expect_equal(bin_choose(5, 1:3), c(5, 10, 10))
  expect_length(bin_choose(5, 3), 1)
  expect_type(bin_choose(5, 3), 'double')
})

test_that("binomial probability works as expected", {
  expect_error(bin_probability(3, -4, 0.4))
  expect_error(bin_probability(7, 4, 0.4))
  expect_error(bin_probability(3, 4, 1.3))
  expect_equal(bin_probability(3, 5, 0.7), 0.3087)
  expect_type(bin_probability(3, 8, 0.3), "double")
  expect_length(bin_probability(3, 9, 0.4), 1)
})

test_that("binomial distribution works as expected", {
  expect_is(bin_distribution(8, 0.4), 'bindis')
  expect_error(bin_distribution(7.4, 0.4))
  expect_is(bin_distribution(8, 0.4), 'data.frame')
  expect_length(bin_distribution(9, 0.3), 2)
})

test_that("binomial cumulative works as expected", {
  expect_is(bin_cumulative(10, 0.4), "bincum")
  expect_is(bin_cumulative(10, 0.4), "data.frame")
  expect_error(bin_cumulative(10, -.1))
  expect_length(bin_cumulative(10, 0.4), 3)
})

