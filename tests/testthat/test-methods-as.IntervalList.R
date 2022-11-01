
## This file is part of the IntervalQuestionStat package for R

## Tests for as.IntervalList() function

test_that("as.IntervalList works", {
  expect_equal(as.IntervalList(IntervalData(0, 1)), IntervalList(0, 1))
})
