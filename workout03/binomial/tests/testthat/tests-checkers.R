context('testing private checker functions')

test_that("check_prob works with a valid probability", {
  expect_true(check_prob(0.1))
  expect_length(check_prob(0.5), 1)
  expect_error(check_prob(-.1))
})

test_that("check_trials works with a valid number of trials", {
  expect_true(check_trials(3))
  expect_error(check_trials(3.4))
  expect_error(check_trials(-3))
})

test_that("check_success works with a valid number of successes", {
  expect_true(check_success(3,5))
  expect_error(check_success(-2,5))
  expect_error(check_success(6,5))
})
