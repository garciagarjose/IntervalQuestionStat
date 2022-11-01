
## This file is part of the IntervalQuestionStat package for R

## Tests for apply() function

data <- matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4)
m <- IntervalMatrix(data)

test_that("apply works", {
  expect_equal(apply(m, 1, mean), IntervalList(c(0, 2), c(3, 6)))
  expect_equal(apply(m, 2, mean), IntervalList(c(0.5, 1.5), c(2.5, 6.5)))
  expect_equal(apply(m, 1, var), c(0.5, 5.0))
  expect_equal(apply(m, 2, var), c(0.25, 4.25))
})

test_that("apply does not work whether MARGIN is not 1 or 2", {
  expect_error(apply(m, 3, var))
})
