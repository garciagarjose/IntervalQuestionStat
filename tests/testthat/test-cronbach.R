
## This file is part of the IntervalQuestionStat package for R

## Tests for cronbach() function

test_that("cronbach works", {
  data <- lackinfo[, 3:12]
  matrix <- IntervalMatrix(data)
  
  ## With likert-type scale responses
  set.seed(1234)
  likert <- ivs2likert(matrix)
  alpha1 <- cronbach(likert, ivs = FALSE)
  expect_equal(alpha1, 0.79082295)
  
  ## With visual analogue scale responses
  vas <- ivs2vas(matrix)
  alpha2 <- cronbach(vas, ivs = FALSE)
  expect_equal(alpha2, 0.79855765)
  
  ## With interval-valued scale responses
  expect_equal(cronbach(data), 0.80666)
  expect_equal(cronbach(data, theta = 1/3), 0.8014514)
})

test_that("cronbach does not work", {
  lower_bounds1 <- c(0, 1, 2, 4)
  upper_bounds1 <- c(2, 3, 4, 5)
  lower_bounds2 <- c(6, 7, 8, 9)
  upper_bounds2<- c(10, 11, 12, 13)
  
  data <- data.frame(mids1 = (upper_bounds1 + lower_bounds1)/2,
                     sprs1 = (upper_bounds1 - lower_bounds1)/2,
                     mids2 = (upper_bounds2 + lower_bounds2)/2,
                     sprs2 = (upper_bounds2 - lower_bounds2)/2)
  
  ## Whether theta is negative
  expect_error(cronbach(data, theta = -1/3))
  
  ## Whether theta is not a single numeric object
  expect_error(cronbach(data, theta = c(1, 1/3)))
  
  ## Whether only a unique interval-valued variable is considered
  expect_error(cronbach(data[, 1:2], type = 2))
  
  ## Whether type is not 1, 2, 3 or 4
  expect_error(cronbach(data, type = 5))
  
  ## Whether data argument is not a matrix or data.frame
  expect_error(cronbach(lower_bounds1))
  
  ## Whether ivs argument is not a single logical value
  expect_error(cronbach(data, ivs = c(TRUE, TRUE)))
  expect_error(cronbach(data, ivs = "true"))
})
