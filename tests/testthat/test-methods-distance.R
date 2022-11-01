
## This file is part of the IntervalQuestionStat package for R

## Tests for distance() function

i1 <- IntervalData(0, 1)
i2 <- IntervalData(3, 7)

test_that("distance works", {
  ## Default options
  expect_equal(distance(i1, i2), 4.7434165)
  
  ## Changing theta
  expect_equal(distance(i1, i2, theta = 1/3), 4.5825757)
})

test_that("distance does not work", {
  ## Whether theta is negative
  expect_error(distance(i1, i2, theta = -1/3))
  
  ## Whether theta is not a single numeric value
  expect_error(distance(i1, i2, theta = c(1, 1/3)))
})
