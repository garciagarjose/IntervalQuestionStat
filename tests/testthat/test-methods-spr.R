
## This file is part of the IntervalQuestionStat package for R

## Tests for spr environment

test_that("spr works", {
  ## With IntervalData
  i1 <- IntervalData(0, 1)
  expect_equal(spr(i1), 0.5)
  
  ## With IntervalList
  l1 <- IntervalList(c(0, 1), c(1, 2))
  expect_equal(spr(l1), c(0.5, 0.5))
  
  ## With IntervalList
  m1 <- IntervalMatrix(matrix(1:8, 2, 4)) 
  expect_equal(spr(m1),
               matrix(1, 2, 2, dimnames = list(1:2, c("spr1", "spr2"))))
})

test_that("spr<- works", {
  ## With IntervalData
  interval <- IntervalData(0, 1)
  spr(interval) <- 0.75
  expect_equal(spr(interval), 0.75)
  
  ## With IntervalList
  list <- IntervalList(c(1, 3, 5), c(3, 5, 7))
  spr(list[c(1, 3)]) <- c(0.75, 2)
  expect_equal(spr(list[c(1, 3)]), c(0.75, 2))
  
  ## With IntervalMatrix
  matrix<- IntervalMatrix(matrix(c(1, 5, 2, 6, 6, 2, 7, 3, 3, 4, 4, 5), 2, 6))
  
  spr(matrix[1, 1]) <- 2
  expect_equal(spr(matrix[1, 1]), 2)
  
  spr(matrix[1, 1:2]) <- c(2, 4)
  expect_equal(unname(spr(matrix[1, 1:2])), c(2, 4))
  
  spr(matrix[1:2, 1:2]) <- matrix(1:4, 2, 2)
  expect_equal(unname(spr(matrix[1:2, 1:2])), matrix(1:4, 2, 2))
})
