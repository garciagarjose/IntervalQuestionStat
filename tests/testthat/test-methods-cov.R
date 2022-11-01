
## This file is part of the IntervalQuestionStat package for R

## Tests for cov() function

list <- IntervalList(c(1, 3), c(2, 5))

test_that("cov works", {
  expect_equal(cov(list, list), var(list))
  expect_equal(cov(list, list, theta = 1/3), var(list, theta = 1/3))
})

test_that("cov does not work", {
  ## Whether theta is negative
  expect_error(cov(list, list, theta = -1/3))
  
  ## Whether theta is not a single numeric value
  expect_error(cov(list, list, theta = c(1, 1/3)))
  
  ## Whether the length of the  given lists of intervals differ
  expect_error(cov(list, list[1]))
})
