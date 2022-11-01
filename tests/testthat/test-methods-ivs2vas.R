
## This file is part of the IntervalQuestionStat package for R

## Tests for ivs2vas() function

test_that("ivs2vas works", {
  ## With IntervalData objects
  i <- IntervalData(3, 5)
  i2vas <- ivs2vas(i)
  expect_equal(i2vas, 4)
  
  ## With IntervalList objects
  list <- IntervalList(c(3, 4), c(5, 6))
  list2vas <- ivs2vas(list)
  expect_equal(list2vas, c(4, 5))
  
  ## With IntervalMatrix objects
  matrix <- IntervalMatrix(matrix(c(3, 2, 5, 4, 1, 1, 5, 3), 2, 4))
  matrix2vas <- ivs2vas(matrix)
  expect_equal(matrix2vas, data.frame(item1 = c(4, 3),
                                      item2 = c(3, 2)))
})
