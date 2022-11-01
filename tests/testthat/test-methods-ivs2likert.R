
## This file is part of the IntervalQuestionStat package for R

## Tests for ivs2likert() function

test_that("ivs2likert works", {
  ## With IntervalData objects
  i <- IntervalData(3, 3.2)
  i2likert <- ivs2likert(i, 11, 0, 10)
  expect_equal(i2likert, 3)
  
  ## With IntervalList objects changing theta
  list <- IntervalList(c(3, 5.7), c(3.2, 6))
  list2likert <- ivs2likert(list, theta = 1/3)
  expect_equal(list2likert, c(3, 6))
  
  ## With IntervalMatrix objects
  data <- matrix(c(3, 4.8, 3.2, 5, 6.9, 1.8, 7.1, 2.2), 2, 4)
  matrix <- IntervalMatrix(data)
  matrix2likert <- ivs2likert(matrix, 11, 0, 10)
  expect_equal(matrix2likert, data.frame(item1 = c(3, 5),
                                         item2 = c(7, 2)))
})

test_that("ivs2likert does not work", {
  ## Whether given interval is not in the reference interval
  i1 <- IntervalData(10, 14)
  expect_error(ivs2likert(i1))
  
  ## Whether k is not a single positive integer
  i2 <- IntervalData(2, 4)
  expect_error(ivs2likert(i2, k = 7.5))
  expect_error(ivs2likert(i2, k = c(1, 7)))
  expect_error(ivs2likert(i2, k = -1))
  
  ## Whether theta is not a single positive real number
  expect_error(ivs2likert(i2, theta = -1/3))
  expect_error(ivs2likert(i2, theta = c(1/3, 1)))
  
  ## Whether minimum or maximum arguments are not single real numbers
  expect_error(ivs2likert(i2, minimum = c(1, 7)))
  expect_error(ivs2likert(i2, maximum = c(5, 7)))
  
  ## Whether minimum > maximum
  expect_error(ivs2likert(i2, minimum = 7, maximum = 1))
})
