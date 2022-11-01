
## This file is part of the IntervalQuestionStat package for R

## Tests for length function

test_that("length works", {
  list <- IntervalList(c(0, 2, 5), c(1, 6, 10))
  expect_equal(length(list), as.integer(3))
})
