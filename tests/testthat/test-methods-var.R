
## This file is part of the IntervalQuestionStat package for R

## Tests for var() function

list <- IntervalList(c(1, 3), c(2, 5))

test_that("var works", {
  ## Default options
  expect_equal(var(list), 1.625)
  
  ## Changing theta
  expect_equal(var(list, theta = 1/3), 1.583333333)
})

test_that("var does not work", {
  ##  Whether theta is negative
  expect_error(var(list, theta = -1/3))
  
  ##  Whether theta is not a single value
  expect_error(var(list, theta = c(1, 1/3)))
})
