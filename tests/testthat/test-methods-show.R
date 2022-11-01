
## This file is part of the IntervalQuestionStat package for R

## Tests for show() functions

test_that("show works", {
  ## With IntervalData
  i <- IntervalData(0, 1)
  output1 <- capture_output_lines(show(i))
  expect_equal(output1, c("An object of class \"IntervalData\" with:",
                          "    inf/sup-characterization: [0,1],",
                          "    mid/spr-characterization: [0.5-+0.5]."))

  ## With IntervalList
  list <- IntervalList(c(0, 1), c(1, 2))
  output2 <- capture_output_lines(show(list))
  expect_equal(output2, c("An object of class \"IntervalList\"",
                          "[[1]]",
                          "An object of class \"IntervalData\" with:",
                          "    inf/sup-characterization: [0,1],",
                          "    mid/spr-characterization: [0.5-+0.5].",
                          "",
                          "[[2]]",
                          "An object of class \"IntervalData\" with:",
                          "    inf/sup-characterization: [1,2],",
                          "    mid/spr-characterization: [1.5-+0.5].",
                          ""))

  ## With IntervalMatrix
  matrix <- IntervalMatrix(matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4))
  output3 <- capture_output_lines(show(matrix))
  expect_equal(output3, paste("An object of class \"IntervalMatrix\"",
                              "with 2 rows and 2 columns."))
})
