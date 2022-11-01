
## This file is part of the IntervalQuestionStat package for R

## Tests for cbind() function

test_that("cbind works", {
  ## All IntervalLists
  list1 <- IntervalList(c(0, 3), c(4, 5))
  list2 <- IntervalList(c(3, 0), c(7, 4))
  expect_equal(cbind(list1, list2),
               IntervalMatrix(matrix(c(0, 3, 4, 5, 3, 0, 7, 4), 2, 4)))
  
  ## All IntervalMatrix
  data1 <- matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4)
  matrix1 <- IntervalMatrix(data1)
  data2 <- matrix(c(1, 5, 2, 6, 0, 1, 2, 3), 2, 4)
  matrix2 <- IntervalMatrix(data2)
  expect_equal(cbind(matrix1, matrix2),
               IntervalMatrix(matrix(c(0, 1, 2, 3, 0, 3, 4, 9,
                                       1, 5, 2, 6, 0, 1, 2, 3), 2, 8)))
  
  
  ## Mixing both IntervalList and IntervalMatrix
  expect_equal(cbind(list1, matrix1),
               IntervalMatrix(matrix(c(0, 3, 4, 5, 0, 1,
                                       2, 3, 0, 3, 4, 9), 2, 6)))
  
})
