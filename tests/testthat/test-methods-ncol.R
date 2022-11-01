
## This file is part of the IntervalQuestionStat package for R

## Tests for ncol() function

test_that("ncol works", {
  m <- IntervalMatrix(matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4))
  expect_equal(ncol(m), as.integer(2))
})
