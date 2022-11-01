
## This file is part of the IntervalQuestionStat package for R

## Tests for mean() function

test_that("mean works", {
  list <- IntervalList(c(2, 4), c(4, 6))
  expect_equal(mean(list), IntervalData(3, 5))
})
