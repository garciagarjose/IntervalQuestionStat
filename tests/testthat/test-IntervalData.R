
## This file is part of the IntervalQuestionStat package for R

## Tests for IntervalData() function

test_that("IntervalData works", {
  # With inf/sup-characterization
  lower_bound <- 0
  upper_bound <- 2
  interval1 <- IntervalData(lower_bound, upper_bound)
  expect_equal(interval1@mid, (upper_bound + lower_bound) / 2)
  expect_equal(interval1@spr, (upper_bound - lower_bound) / 2)
  
  ## With mid/spr-characterization
  midpoint <- 1
  spread <- 1
  interval2 <- IntervalData(midpoint, spread, type = 2)
  expect_equal(interval2@mid, midpoint)
  expect_equal(interval2@spr, spread)
})

test_that("IntervalData does not work", {
  ## Whether inf > sup
  lower_bound <- 2
  upper_bound <- 0
  expect_error(IntervalData(lower_bound, upper_bound))
  
  # Whether spr < 0
  midpoint <- 1
  spread <- -2
  expect_error(IntervalData(midpoint, spread, type = 2))
  
  ## Whether type is not 1 or 2", {
  expect_error(IntervalData(lower_bound, upper_bound, type = 3))
  
  ## Whether a1 or a2 is not a single numeric value
  expect_error(IntervalData(c(lower_bound, upper_bound), upper_bound, type = 1))
  expect_error(IntervalData(lower_bound, c(lower_bound, upper_bound), type = 1))
})
