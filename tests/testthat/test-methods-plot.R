
## This file is part of the IntervalQuestionStat package for R

## Tests for plot() function

i1 <- IntervalData(0, 1)
i2 <- IntervalData(2, 3)
list1 <- IntervalList(c(0, 3, 2, 5, 6), c(4, 5, 4, 8, 7))
list2 <- IntervalList(c(3, 0, 3, 1, 4), c(7, 4, 6, 2, 6))

test_that("plot works for a single vertical interval with mid and bounds", {
  p1 <- function() plot(i1, bounds = TRUE, mid = TRUE)
  vdiffr::expect_doppelganger("single-vertical-interval-mid-bound", p1)
})

test_that("plot works for a single horizontal interval", {
  p2 <- function() plot(i1, layout= "horizontal")
  vdiffr::expect_doppelganger("single-horizontal-interval", p2)
})

test_that("plot works for a single red interval", {
  p3 <- function() plot(i1, col = "red")
  vdiffr::expect_doppelganger("single-red-intervals", p3)
})

test_that("plot works for two default intervals", {
  p4 <- function() plot(i1, i2)
  vdiffr::expect_doppelganger("two-intervals-default", p4)
})

test_that("plot works for two intervals with bounds", {
  p5 <- function() plot(i1, i2, bounds = TRUE)
  vdiffr::expect_doppelganger("two-intervals-bounds", p5)
})

test_that("plot works for a single vertical list", {
  p6 <- function() plot(list1)
  vdiffr::expect_doppelganger("single-vertical-list", p6)
})

test_that("plot works for a single vertical list with mid and bounds", {
  p7 <- function() plot(list1, layout = "horizontal",
                        bounds = TRUE, mid = TRUE)
  vdiffr::expect_doppelganger("single-horizontal-list-bounds-mid", p7)
})

test_that("plot works for two default lists", {
  p8 <- function() plot(list1, list2)
  vdiffr::expect_doppelganger("two-lists-default", p8)
})

test_that("plot works for two lists with bounds", {
  p9 <- function() plot(list1, list2, bounds = TRUE)
  vdiffr::expect_doppelganger("two-lists-bounds", p9)
})

test_that("plot works for two customized lists", {
  p10 <- function() plot(list1, list2, bounds = TRUE,
                         main = "My interval-valued data plot",
                         col = "blue", lwd = 2,
                         xlab = "My xlab", ylab = "My ylab")
  vdiffr::expect_doppelganger("two-lists-customized", p10)
})

test_that("plots does not work", {
  ## Whether list sizes differ
  list1 <- IntervalList(c(0, 3, 2, 5, 6), c(4, 5, 4, 8, 7))
  list2 <- IntervalList(c(3, 0, 3, 1), c(7, 4, 6, 2))
  expect_error(plot(list1, list2))
  
  ## Whether bound and mid are not single logical values
  list1 <- IntervalList(c(0, 3, 2, 5, 6), c(4, 5, 4, 8, 7))
  list2 <- IntervalList(c(3, 0, 3, 1, 4), c(7, 4, 6, 2, 8))
  expect_error(plot(list1, bounds = "true"))
  expect_error(plot(list1, mid = "true"))
  expect_error(plot(list1, list2, bounds = "true"))
  expect_error(plot(list1, bounds = c(TRUE, TRUE)))
  expect_error(plot(list1, mid = c(TRUE, TRUE)))
  expect_error(plot(list1, list2, bounds = c(TRUE, TRUE)))
})
