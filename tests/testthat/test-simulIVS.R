
## This file is part of the IntervalQuestionStat package for R

## Tests for simulIVS() function

test_that("simulIVS works", {
  data <- simulIVS(93, 5)
  expect_equal(class(data), "data.frame")
  expect_equal(nrow(data), 93)
  expect_equal(ncol(data), 10)
})

test_that("simulIVS does not works", {
  ## Whether n is not a single positive integer
  expect_error(simulIVS(n = c(1, 2), k = 10))
  expect_error(simulIVS(n = -10, k = 10))
  expect_error(simulIVS(n = 7.5, k = 10))
  
  ## Whether n is not a single positive integer
  expect_error(simulIVS(n = 10, k = c(1, 2)))
  expect_error(simulIVS(n = 10, k = -10))
  expect_error(simulIVS(n = 10, k = 7.5))
  
  ## Whether w1 is not a single real number in [0, 1]
  expect_error(simulIVS(n = 10, k = 10, w1 = c(0.25, 0.5)))
  expect_error(simulIVS(n = 10, k = 10, w1 = -0.5))
  expect_error(simulIVS(n = 10, k = 10, w1 = 1.5))
  
  ## Whether w2 is not a single real number in [0, 1]
  expect_error(simulIVS(n = 10, k = 10, w2 = c(0.25, 0.5)))
  expect_error(simulIVS(n = 10, k = 10, w2 = -0.5))
  expect_error(simulIVS(n = 10, k = 10, w2 = 1.5))
  
  ## Whether w3 is not a single real number in [0, 1]
  expect_error(simulIVS(n = 10, k = 10, w3 = c(0.25, 0.5)))
  expect_error(simulIVS(n = 10, k = 10, w3 = -0.5))
  expect_error(simulIVS(n = 10, k = 10, w3 = 1.5))
  
  ## Whether w1 + w2 + w3 = 1 is not fulfilled
  expect_error(simulIVS(100, 5, 0.5, 0.5, 0.5))
  
  ## Whether p is not a single non-negative real number
  expect_error(simulIVS(n = 10, k = 10, p = c(1, 7)))
  expect_error(simulIVS(n = 10, k = 10, p = -2))
  
  ## Whether p is not a single non-negative real number
  expect_error(simulIVS(n = 10, k = 10, q = c(1, 7)))
  expect_error(simulIVS(n = 10, k = 10, q = -2))
  
  ## Whether minimum or maximum arguments are not single real numbers
  expect_error(simulIVS(n = 10, k = 10, minimum = c(1, 7)))
  expect_error(simulIVS(n = 10, k = 10, maximum = c(5, 7)))
  
  ## Whether minimum > maximum
  expect_error(simulIVS(n = 10, k = 10, minimum = 7, maximum = 1))
})
