
## This file is part of the IntervalQuestionStat package for R

## Tests for arithmetic environment

i1 <- IntervalData(0, 1)
i2 <- IntervalData(1, 2)

l1 <- IntervalList(c(0, 1), c(1, 2))
l2 <- IntervalList(c(1, 2), c(2, 3))

m1 <- IntervalMatrix(matrix(c(0, 1, 1, 2, 2, 3, 3, 4), 2, 4))
m2 <- IntervalMatrix(matrix(c(0, 2, 1, 3, 4, 6, 5, 7), 2, 4))

test_that("+ works with IntervalData", {
  ## With two intervals
  expect_equal(i1 + i2, IntervalData(1, 3))
  
  ## With an interval and a right-sided number
  expect_equal(i1 + 1, i2)
  
  ## With an interval and a left-sided number
  expect_equal(1 + i1, i2)
})

test_that("- works with IntervalData", {
  ## With two intervals
  expect_equal(i1 - i2, IntervalData(-2, 0))
  
  ## With an interval and a right-sided number
  expect_equal(i1 - 1, IntervalData(-1, 0))
  
  ## With an interval and a left-sided number
  expect_equal(1 - i1, i1)
  
  ## When there is only one interval
  expect_equal(- i2, IntervalData(-2, -1))
})

test_that("* works with IntervalData", {
  ## On the left
  expect_equal(2 * i2, IntervalData(2, 4))
  
  ## On the right
  expect_equal(i2 * 2, IntervalData(2, 4))
})

test_that("+ works with IntervalList", {
  ## With two list of intervals
  expect_equal(l1 + l2, IntervalList(c(1, 3), c(3, 5)))
  
  ## With a list of intervals and a right-sided number
  expect_equal(l1 + 1, l2)
  
  ## With a list of intervals and a left-sided number
  expect_equal(1 + l1, l2)
  
  ## With a list of intervals and a right-sided interval
  expect_equal(l1 + i1, IntervalList(c(0, 1), c(2, 3)))
  
  ## With a list of intervals and a left-sided interval
  expect_equal(i1 + l1, IntervalList(c(0, 1), c(2, 3)))
})

test_that("- works with IntervalList", {
  ## With two lists of intervals
  expect_equal(l1 - l2, IntervalList(c(-2, -2), c(0, 0)))
  
  ## With a list of intervals and a right-sided number
  expect_equal(l1 - 1, IntervalList(c(-1, 0), c(0, 1)))
  
  ## With a list of intervals and a left-sided number
  expect_equal(1 - l1, IntervalList(c(0, -1), c(1, 0)))
  
  ## With a list of intervals and a right-sided interval
  expect_equal(l1 - i1, IntervalList(c(-1, 0), c(1, 2)))
  
  ## With a list of intervals and a left-sided interval
  expect_equal(i1 - l1, IntervalList(c(-1, -2), c(1, 0)))
  
  ## When there is only one list of intervals
  expect_equal(- l1, IntervalList(c(-1, -2), c(0, -1)))
})

test_that("* works with IntervalList", {
  ## On the left
  expect_equal(2 * l1, IntervalList(c(0, 2), c(2, 4)))
  
  ## On the right
  expect_equal(l1 * 2, IntervalList(c(0, 2), c(2, 4)))
})

test_that("+ works with IntervalMatrix", {
  ## With two matrices of intervals
  expect_equal(m1 + m2,
               IntervalMatrix(matrix(c(0, 3, 2, 5, 6, 9, 8, 11), 2, 4)))
  
  ## With a matrix of intervals and a right-sided number
  expect_equal(m1 + 1,
               IntervalMatrix(matrix(c(1, 2, 2, 3, 3, 4, 4, 5), 2, 4)))
  
  ## With a matrix of intervals and a left-sided number
  expect_equal(1 + m1,
               IntervalMatrix(matrix(c(1, 2, 2, 3, 3, 4, 4, 5), 2, 4)))
  
  ## With a matrix of intervals and a right-sided matrix
  expect_equal(m1 + matrix(1, 2, 2),
               IntervalMatrix(matrix(c(1, 2, 2, 3, 3, 4, 4, 5), 2, 4)))
  
  ## With a matrix of intervals and a left-sided matrix
  expect_equal(matrix(1, 2, 2) + m1,
               IntervalMatrix(matrix(c(1, 2, 2, 3, 3, 4, 4, 5), 2, 4)))
  
  ## With a matrix of intervals and a right-sided interval
  expect_equal(m1 + i1,
               IntervalMatrix(matrix(c(0, 1, 2, 3, 2, 3, 4, 5), 2, 4)))
  
  ## With a matrix of intervals and a left-sided interval
  expect_equal(i1 + m1,
               IntervalMatrix(matrix(c(0, 1, 2, 3, 2, 3, 4, 5), 2, 4)))
  
  ## With a matrix of intervals and a right-sided list of intervals
  expect_equal(m1 + l1,
               IntervalMatrix(matrix(c(0, 2, 2, 4, 2, 4, 4, 6), 2, 4)))
  
  ## With a matrix of intervals and a left-sided list of intervals
  expect_equal(l1 + m1,
               IntervalMatrix(matrix(c(0, 2, 2, 4, 2, 4, 4, 6), 2, 4)))
})

test_that("- works with IntervalMatrix", {
  ## With two matrices of intervals
  expect_equal(m1 - m2,
               IntervalMatrix(matrix(c(-1, -2, 1, 0, -3, -4, -1, -2), 2, 4)))
  
  ## With a matrix of intervals and a right-sided number
  expect_equal(m1 - 1,
               IntervalMatrix(matrix(c(-1, 0, 0, 1, 1, 2, 2, 3), 2, 4)))
  
  ## With a matrix of intervals and a left-sided number
  expect_equal(1 - m1,
               IntervalMatrix(matrix(c(0, -1, 1, 0, -2, -3, -1, -2), 2, 4)))
  
  ## With a matrix of intervals and a right-sided matrix
  expect_equal(m1 - matrix(1, 2, 2),
               IntervalMatrix(matrix(c(-1, 0, 0, 1, 1, 2, 2, 3), 2, 4)))
  
  ## With a matrix of intervals and a left-sided matrix
  expect_equal(matrix(1, 2, 2) - m1,
               IntervalMatrix(matrix(c(0, -1, 1, 0, -2, -3, -1, -2), 2, 4)))
  
  ## With a matrix of intervals and a right-sided interval
  expect_equal(m1 - i1,
               IntervalMatrix(matrix(c(-1, 0, 1, 2, 1, 2, 3, 4), 2, 4)))
  
  ## With a matrix of intervals and a left-sided interval
  expect_equal(i1 - m1,
               IntervalMatrix(matrix(c(-1, -2, 1, 0, -3, -4, -1, -2), 2, 4)))
  
  ## With a matrix of intervals and a right-sided list of intervals
  expect_equal(m1 - l1,
               IntervalMatrix(matrix(c(-1, -1, 1, 1, 1, 1, 3, 3), 2, 4)))

  ## With a matrix of intervals and a left-sided list of intervals
  expect_equal(l1 - m1,
               IntervalMatrix(matrix(c(-1, -1, 1, 1, -3, -3, -1, -1), 2, 4)))
  
  ## When there is only one matrix of intervals
  expect_equal(- m2,
               IntervalMatrix(matrix(c(-1, -3, 0, -2, -5, -7, -4, -6), 2, 4)))
})

test_that("* works with IntervalMatrix", {
  ## With numeric on the left
  expect_equal(2 * m1,
               IntervalMatrix(matrix(c(0, 2, 2, 4, 4, 6, 6, 8), 2, 4)))
  
  ## With numeric on the right
  expect_equal(m1 * 2,
               IntervalMatrix(matrix(c(0, 2, 2, 4, 4, 6, 6, 8), 2, 4)))

  ## With matrix on the left
  expect_equal(matrix(2, 2, 2) * m1,
               IntervalMatrix(matrix(c(0, 2, 2, 4, 4, 6, 6, 8), 2, 4)))

  ## With matrix on the right
  expect_equal(m1 * matrix(2, 2, 2),
               IntervalMatrix(matrix(c(0, 2, 2, 4, 4, 6, 6, 8), 2, 4)))
})
