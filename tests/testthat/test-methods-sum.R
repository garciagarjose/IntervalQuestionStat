
## This file is part of the IntervalQuestionStat package for R

## Tests for sum() function

test_that("sum works", {
  list <- IntervalList(c(2, 4), c(4, 6))
  expect_equal(sum(list), IntervalData(6, 10))
})

test_that("prod does not work", {
  list <- IntervalList(c(2, 4), c(4, 6))
  expect_error(prod(list))
})
