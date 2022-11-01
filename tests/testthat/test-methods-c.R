
## This file is part of the IntervalQuestionStat package for R

## Tests for c() function

test_that("c works", {
  ##  With IntervalData
  expect_equal(c(IntervalData(0, 1), IntervalData(1, 2)),
               IntervalList(c(0, 1), c(1, 2)))
  
  ## With IntervalList
  expect_equal(c(IntervalList(c(0, 1), c(1, 2)),
                 IntervalList(c(2, 3), c(3, 4))),
               IntervalList(c(0, 1, 2, 3), c(1, 2, 3, 4)))
  
  ## With IntervalData and IntervalList
  expect_equal(c(IntervalData(0, 1), IntervalList(c(0, 1), c(1, 2))),
               IntervalList(c(0, 0, 1), c(1, 1, 2)))
  
  ## With IntervalList and IntervalData
  expect_equal(c(IntervalList(c(0, 1), c(1, 2)), IntervalData(0, 1)),
               IntervalList(c(0, 1, 0), c(1, 2, 1)))
})
