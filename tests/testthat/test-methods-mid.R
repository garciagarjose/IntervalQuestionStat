
## This file is part of the IntervalQuestionStat package for R

## Tests for mid environment

test_that("mid works", {
  ## With IntervalData
  i1 <- IntervalData(0, 1)
  expect_equal(mid(i1), 0.5)
  
  ## With IntervalList
  l1 <- IntervalList(c(0, 1), c(1, 2))
  expect_equal(mid(l1), c(0.5, 1.5))
  
  ## With IntervalList
  m1 <- IntervalMatrix(matrix(1:8, 2, 4)) 
  expect_equal(mid(m1),
               matrix(c(2, 3, 6, 7), 2, 2,
                      dimnames = list(1:2, c("mid1", "mid2"))))
})

test_that("mid<- works", {
  ## With IntervalData
  interval <- IntervalData(0, 1)
  mid(interval) <- 0.75
  expect_equal(mid(interval), 0.75)
  
  ## With IntervalList
  list <- IntervalList(c(1, 3, 5), c(3, 5, 7))
  mid(list)[c(1, 3)] <- c(0.75, 2)
  expect_equal(mid(list[c(1, 3)]), c(0.75, 2))
  
  ## With IntervalMatrix
  matrix<- IntervalMatrix(matrix(c(1, 5, 2, 6, 6, 2, 7, 3, 3, 4, 4, 5), 2, 6))
  
  mid(matrix[1, 1]) <- 2
  expect_equal(mid(matrix[1, 1]), 2)
  
  mid(matrix[1, 1:2]) <- c(2, 4)
  expect_equal(unname(mid(matrix[1, 1:2])), c(2, 4))
  
  mid(matrix[1:2, 1:2]) <- matrix(1:4, 2, 2)
  expect_equal(unname(mid(matrix[1:2, 1:2])), matrix(1:4, 2, 2))
})
