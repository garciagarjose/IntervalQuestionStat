
## This file is part of the IntervalQuestionStat package for R

## Tests for nrow() function

test_that("nrow works", {
  m <- IntervalMatrix(matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4))
  expect_equal(nrow(m), as.integer(2))
})
