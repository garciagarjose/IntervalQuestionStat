
## This file is part of the IntervalQuestionStat package for R

## Tests for rbind() function

test_that("rbind works", {
  ## All IntervalLists
  list1 <- IntervalList(c(0, 3), c(4, 5))
  list2 <- IntervalList(c(3, 0), c(7, 4))
  expect_equal(rbind(list1, list2),
               IntervalMatrix(matrix(c(0, 3, 4, 7, 3, 0, 5, 4), 2, 4)))
  
  ## All IntervalMatrix
  data1 <- matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4)
  matrix1 <- IntervalMatrix(data1)
  data2 <- matrix(c(1, 5, 2, 6, 0, 1, 2, 3), 2, 4)
  matrix2 <- IntervalMatrix(data2)
  expect_equal(rbind(matrix1, matrix2),
               IntervalMatrix(matrix(c(0, 1, 1, 5,
                                       2, 3, 2, 6,
                                       0, 3, 0, 1,
                                       4, 9, 2, 3), 4, 4)))
  
  
  ## Mixing both IntervalList and IntervalMatrix
  expect_equal(rbind(list1, matrix1),
               IntervalMatrix(matrix(c(0, 0, 1, 4, 2, 3,
                                       3, 0, 3, 5, 4, 9), 3, 4)))
  
})
