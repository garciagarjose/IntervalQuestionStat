
## This file is part of the IntervalQuestionStat package for R

## Tests for extract environment

test_that("[ works", {
  ## With IntervalList objects 
  list <- IntervalList(c(1, 3, 5), c(2, 4, 6))
  expect_equal(list[1], IntervalList(1, 2))
  expect_equal(list[c(1, 3)], IntervalList(c(1, 5), c(2, 6)))
  
  ## With IntervalMatrix objects
  matrix <- IntervalMatrix(matrix(c(1, 5, 2, 6, 6, 2, 7, 3, 3, 4, 4, 5), 2, 6))
  expect_equal(matrix[1, 1], IntervalData(1, 2))
  expect_equal(matrix[1, ], IntervalList(c(1, 6, 3), c(2, 7, 4)))
  expect_equal(matrix[, 1], IntervalList(c(1, 5), c(2, 6)))
  expect_equal(matrix[, c(1, 3)],
               IntervalMatrix(matrix(c(1, 5, 2, 6, 3, 4, 4, 5), 2, 4)))
})

test_that("[[ works", {
  list <- IntervalList(c(1, 3, 5), c(2, 4, 6))
  expect_equal(list[[1]], IntervalData(1, 2))
})

test_that("[[ does not work", {
  list <- IntervalList(c(1, 3, 5), c(2, 4, 6))
  expect_error(list[[c(1, 3)]])
})

test_that("[<- and [[<- works", {
  ## With a list of intervals
  list <- IntervalList(c(1, 3, 5), c(2, 4, 6))
  
  list[[1]] <- IntervalData(0, 1)
  expect_equal(list, IntervalList(c(0, 3, 5), c(1, 4, 6)))
  
  list[c(1, 3)] <- IntervalList(c(0, 1), c(1, 2))
  expect_equal(list, IntervalList(c(0, 3, 1), c(1, 4, 2)))
  
  ## With a matrix of intervals
  m <- IntervalMatrix(matrix(c(1, 5, 2, 6, 6, 2, 7, 3, 3, 4, 4, 5), 2, 6))
  
  m[1, 1] <- IntervalData(0, 1)
  expect_equal(m, IntervalMatrix(matrix(c(0, 5, 1, 6, 6, 2, 7, 3, 3, 4, 4, 5),
                                        2, 6)))
  
  m[1, ] <- IntervalList(c(0, 1, 2), c(1, 2, 3))
  expect_equal(m, IntervalMatrix(matrix(c(0, 5, 1, 6, 1, 2, 2, 3, 2, 4, 3, 5),
                                        2, 6)))
  
  m[, c(1, 3)] <- IntervalMatrix(matrix(1:8, 2, 4))
  expect_equal(m, IntervalMatrix(matrix(c(1:4, 1, 2, 2, 3, 5:8), 2, 6)))
})

test_that("[[<- works does not work", {
  ## With a negative integer
  list <- IntervalList(c(1, 3, 5), c(2, 4, 6))
  expect_error(list[[-1]] <- IntervalData(0, 1))
  
  ## When dimensions do not match
  expect_error(eval(parse(text = "list[[1]] <- 0")))
})

