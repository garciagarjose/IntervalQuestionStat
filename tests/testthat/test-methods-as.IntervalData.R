
## This file is part of the IntervalQuestionStat package for R

## Tests for as.IntervalData() function

test_that("as.IntervalData works", {
  expect_equal(as.IntervalData(1), IntervalData(1, 1))
})

test_that("as.IntervalData does not work", {
  expect_error(as.IntervalData(c(1, 2)))
})
