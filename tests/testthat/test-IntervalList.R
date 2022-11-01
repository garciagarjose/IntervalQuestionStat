
## This file is part of the IntervalQuestionStat package for R

## Tests for IntervalList() function

test_that("IntervalList works", {
  ##  With inf/sup-characterization
  lower_bounds <- c(0, 1, 2)
  upper_bounds <- c(2, 3, 4)
  list <- IntervalList(lower_bounds, upper_bounds)
  expect_equal(list@mid, (upper_bounds + lower_bounds) / 2)
  expect_equal(list@spr, (upper_bounds - lower_bounds) / 2)
  
  ##  With mid/spr-characterization
  midpoints <- c(0, 1, 2)
  spreads <- c(2, 3, 4)
  list <- IntervalList(midpoints, spreads, type = 2)
  expect_equal(list@mid, midpoints)
  expect_equal(list@spr, spreads)
})

test_that("IntervalList does not work", {
  ## Whether x and y lengths differ
  lower_bounds1 <- c(0, 1, 2, 3)
  upper_bounds1 <- c(2, 3, 4)
  expect_error(IntervalList(lower_bounds1, upper_bounds1))
  
  ## Whether type is not 1 or 2
  lower_bounds2 <- c(0, 1, 2)
  upper_bounds2 <- c(2, 3, 4)
  expect_error(IntervalList(lower_bounds2, upper_bounds2, type = 3))
  
  ## Whether the unique input output is not a data.frame or a matrix
  expect_error(IntervalList(lower_bounds1))
  
  ## Whether the unique input argument (data.frame or matrix) has no 2 cols
  lower_bounds3a <- c(0, 1, 2, 4)
  upper_bounds3a <- c(2, 3, 4, 5)
  lower_bounds3b <- c(6, 7, 8, 9)
  upper_bounds3b <- c(10, 11, 12, 13)
  data <- data.frame(infs1 = lower_bounds3a,
                     sups1 = upper_bounds3a,
                     infs2 = lower_bounds3b,
                     sups2 = upper_bounds3b)
  expect_error(IntervalList(data))
  
  ## Whether any lower bound is greater than its corresponding upper bound
  lower_bounds4 <- c(0, 1, 2)
  upper_bounds4 <- c(2, 3, 1)
  expect_error(IntervalList(lower_bounds4, upper_bounds4))
  
  ## Whether any spread is negative
  midpoints <- c(0, 1, 2)
  spreads <- c(3, -2, 3)
  expect_error(IntervalList(midpoints, spreads, type = 2))
})
