
## This file is part of the IntervalQuestionStat package for R

## Tests for IntervalMatrix() function

test_that("IntervalMatrix works", {
  lower_bounds1 <- c(0, 1, 2, 4)
  upper_bounds1 <- c(2, 3, 4, 5)
  lower_bounds2 <- c(6, 7, 8, 9)
  upper_bounds2 <- c(10, 11, 12, 13)
  
  data1 <- data.frame(infs1 = lower_bounds1,
                      sups1 = upper_bounds1,
                      infs2 = lower_bounds2,
                      sups2 = upper_bounds2)
  matrix1 <- IntervalMatrix(data1)
  expect_equal(nrow(matrix1), 4)
  expect_equal(ncol(matrix1), 2)
  
  data2 <- data.frame(mids1 = (upper_bounds1 + lower_bounds1)/2,
                      sprs1 = (upper_bounds1 - lower_bounds1)/2,
                      mids2 = (upper_bounds2 + lower_bounds2)/2,
                      sprs2 = (upper_bounds2 - lower_bounds2)/2)
  matrix2 <- IntervalMatrix(data2, type = 2)
  expect_equal(nrow(matrix2), 4)
  expect_equal(ncol(matrix2), 2)
  
  data3 <- data.frame(infs1 = lower_bounds1,
                      infs2 = lower_bounds2,
                      sups1 = upper_bounds1,
                      sups2 = upper_bounds2)
  matrix3 <- IntervalMatrix(data3, type = 3)
  expect_equal(nrow(matrix3), 4)
  expect_equal(ncol(matrix3), 2)
  
  data4 <- data.frame(mids1 = (upper_bounds1 + lower_bounds1)/2,
                      mids2 = (upper_bounds2 + lower_bounds2)/2,
                      sprs1 = (upper_bounds1 - lower_bounds1)/2,
                      sprs2 = (upper_bounds2 - lower_bounds2)/2)
  matrix4 <- IntervalMatrix(data4, type = 4)
  expect_equal(nrow(matrix4), 4)
  expect_equal(ncol(matrix4), 2)
})

test_that("IntervalMatrix does not work", {
  lower_bounds1 <- c(0, 1, 2, 4)
  upper_bounds1 <- c(2, 3, 4, 5)
  lower_bounds2 <- c(6, 7, 8, 9)
  upper_bounds2<- c(10, 11, 12, 13)
  
  data <- data.frame(infs1 = lower_bounds1,
                     sups1 = upper_bounds1,
                     infs2 = lower_bounds2,
                     sups2 = upper_bounds2)
  
  ## Whether type is not 1, 2, 3 or 4
  expect_error(IntervalMatrix(data, type = 5))
  
  ## Whether the argument given as input is not a matrix or a data.frame
  expect_error(IntervalMatrix(lower_bounds1))
  
  ## Whether the data.frame or matrix given as input column number is not even
  expect_error(IntervalMatrix(data[, 1:3]))
})
