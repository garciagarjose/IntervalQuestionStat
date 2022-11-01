
## This file is part of the IntervalQuestionStat package for R

## Tests for dim() function

test_that("dim works", {
  m <- IntervalMatrix(matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4))
  expect_equal(dim(m), as.integer(c(2, 2)))
})
