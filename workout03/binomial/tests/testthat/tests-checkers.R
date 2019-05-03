context('testing private checker functions')

test_that("check_prob works with a valid probability", {
  expect_true(check_prob(0.1))
  expect_length(check_prob())
  expect_error(check_prob(-.1))
})
