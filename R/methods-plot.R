
## This file is part of the IntervalQuestionStat package

#' @title
#' Plot an interval-valued data or a list of interval-valued data
#'
#' @description
#' S4 methods for function plot.
#' As in the generic plot S3 `graphics' method, these methods plot interval-valued data contained
#' in \code{IntervalData} and \code{IntervalList} objects.
#'
#' @usage
#' \S4method{plot}{IntervalData,missing}(x, y, layout = c("vertical","horizontal"), bounds = FALSE, mid = FALSE, \dots)
#' \S4method{plot}{IntervalData,IntervalData}(x, y, bounds = FALSE, \dots)
#' \S4method{plot}{IntervalList,missing}(x, y, layout = c("vertical","horizontal"), bounds = FALSE, mid = FALSE, \dots)
#' \S4method{plot}{IntervalList,IntervalList}(x, y, bounds = FALSE, \dots)
#'
#' @param x an object of type \code{IntervalData} or \code{IntervalList} representing the values of an interval-value variable.
#' @param y an object of type \code{IntervalData} or \code{IntervalList} representing the values of a second interval-value variable, to be displayed along y (vertical) coordinates.
#' @param layout the axes along which the interval-valued variables be displayed. Alternatives are "vertical" (default) and "horizontal".
#' @param bounds a logical value indicating if interval bounds should be plotted or not (default).
#' @param mid a logical value indicating if the interval mid-points should be plotted or not (default).
#' @param \dots graphical arguments to be passed to methods.
#'
#' @exportMethod plot
#' @docType methods
#' @name plot
#' @family IntervalData-method
#' @family IntervalList-method
#'
#' @rdname plot-methods
#'
#' @aliases plot,IntervalData,missing-method
#'          plot,IntervalData,IntervalData-method
#'          plot,IntervalList,missing-method
#'          plot,IntervalList,IntervalList-method
#'          
#' @return
#' This function does not return any value. It only plots interval-valued data.
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' i1 <- IntervalData(0, 1)
#' i2 <- IntervalData(2, 3)
#' plot(i1)
#' plot(i1, bounds = TRUE, mid = TRUE)
#' plot(i1, i2)
#' plot(i1, i2, bounds = TRUE)
#'
#' list1 <- IntervalList(c(0, 3, 2, 5, 6), c(4, 5, 4, 8, 7))
#' list2 <- IntervalList(c(3, 0, 3, 1, 4), c(7, 4, 6, 2, 6))
#' plot(list1)
#' plot(list1, layout = "horizontal")
#' plot(list1, bounds = TRUE, mid = TRUE)
#' plot(list1, list2)
#' plot(list1, list2, bounds = TRUE)
#'
#' ## Extra arguments
#' plot(list1, list2, bounds = TRUE, main = "My interval-valued data plot", col = "blue", lwd = 2)

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

setMethod("plot",
          signature(x = "IntervalList", y = "missing"),
          function(x, y, layout = c("vertical","horizontal"), bounds = FALSE, mid = FALSE,  ...)
          {
            dotarguments <- match.call(expand.dots = FALSE)$...

            if (is.null(dotarguments$main))
            {
              dotarguments$main <- "Interval-valued data plot"
            }

            layout <- match.arg(layout)

            if (is.null(dotarguments$ylab))
            {
              if (layout == "vertical") dotarguments$ylab <- ""
              else dotarguments$ylab <- "Observation number"
            }
            if (is.null(dotarguments$xlab))
            {
              if (layout=="vertical") dotarguments$xlab <- "Observation number"
              else dotarguments$xlab <- ""
            }

            midpoints <- sapply(x, slot, "mid")
            spreads <- sapply(x, slot, "spr")
            infs <- midpoints - spreads
            sups <- midpoints + spreads
            xcord <- 1:length(x)

            if (is.null(dotarguments$ylim) && layout == "vertical")
            {
              miny <- min(infs)
              maxy <- max(sups)
              dotarguments$ylim <- c(miny-0.05*(maxy-miny), maxy+0.05*(maxy-miny))
            }
            if (is.null(dotarguments$xlim) && layout == "horizontal")
            {
              minx <- min(infs)
              maxx <- max(sups)
              dotarguments$xlim <- c(minx-0.05*(maxx-minx), maxx+0.05*(maxx-minx))
            }

            if (is.null(dotarguments$col))
            {
              dotarguments$col <- "black"
            }

            obsnumber <- c(1, length(x))

            if (layout == "vertical")
            {
              do.call("plot.default",
                      c(list(x = obsnumber, y = rep(0., length(obsnumber)), type = "n"), dotarguments))
              if (bounds == FALSE)
              {
                do.call("segments",
                        c(list(x0 = xcord, y0 = infs, x1 = xcord, y1 = sups), dotarguments))
              } else {
                do.call("arrows",
                        c(list(x0 = xcord, y0 = infs, x1 = xcord, y1 = sups), length = 0.05, angle = 90, code = 3, dotarguments))
              }
              if (mid == TRUE)
              {
                points(xcord, midpoints, pch = 16, col = dotarguments$col)
              }
            }
            else if (layout == "horizontal")
            {
              do.call("plot.default", c(list(x = rep(0., length(obsnumber)), y = obsnumber, type = "n"), dotarguments))
              if (bounds == FALSE)
              {
                do.call("segments",
                        c(list(y0 = xcord, x0 = infs, y1 = xcord, x1 = sups), dotarguments))
              } else {
                do.call("arrows",
                        c(list(y0 = xcord, x0 = infs, y1 = xcord, x1 = sups), length = 0.05, angle = 90, code = 3, dotarguments))
              }

              if (mid == TRUE)
              {
                points(midpoints, xcord, pch = 16, col = dotarguments$col)
              }
            }
          }
)

setMethod("plot",
          signature(x = "IntervalList", y = "IntervalList"),
          function(x, y, bounds = FALSE, ...)
          {
            if (length(x) != length(y)) stop("'x' and 'y' arguments must be of the same length")

            dotarguments <- match.call(expand.dots=FALSE)$...

            if (is.null(dotarguments$main))
            {
              dotarguments$main <- "Interval-valued data plot"
            }
            if (is.null(dotarguments$xlab))
            {
              dotarguments$xlab <- "First interval-valued data list"
            }
            if (is.null(dotarguments$ylab))
            {
              dotarguments$ylab <- "Second interval-valued data list"
            }


            midpointsx <- sapply(x, slot, "mid")
            spreadsx <- sapply(x, slot, "spr")
            infsx <- midpointsx - spreadsx
            supsx <- midpointsx + spreadsx
            midpointsy <- sapply(y, slot, "mid")
            spreadsy <- sapply(y, slot, "spr")
            infsy <- midpointsy - spreadsy
            supsy <- midpointsy + spreadsy

            if (is.null(dotarguments$xlim))
            {
              minx <- min(infsx)
              maxx <-max(supsx)
              dotarguments$xlim <- c(minx-0.05*(maxx-minx), maxx+0.05*(maxx-minx))
            }
            if (is.null(dotarguments$ylim))
            {
              miny <- min(infsy)
              maxy <-max(supsy)
              dotarguments$ylim <- c(miny-0.05*(maxy-miny), maxy+0.05*(maxy-miny))
            }

            if (is.null(dotarguments$col))
            {
              dotarguments$col <- "black"
            }

            do.call("plot.default",
                    c(list(x = 0., y = 0., type = "n"), dotarguments))

            if (bounds == FALSE)
            {
              do.call("segments",
                      c(list(x0 = infsx, y0 = midpointsy, x1 = supsx, y1 = midpointsy), dotarguments))
              do.call("segments",
                      c(list(x0 = midpointsx, y0 = infsy, x1 = midpointsx, y1 = supsy), dotarguments))
            } else {
              do.call("arrows",
                      c(list(x0 = infsx, y0 = midpointsy, x1 = supsx, y1 = midpointsy), length = 0.05, angle = 90, code = 3, dotarguments))
              do.call("arrows",
                      c(list(x0 = midpointsx, y0 = infsy, x1 = midpointsx, y1 = supsy), length = 0.05, angle = 90, code = 3, dotarguments))
            }

            points(midpointsx, midpointsy, pch = 16, col = dotarguments$col)

          }
)

setMethod("plot",
          signature(x = "IntervalData", y = "missing"),
          function(x, y, layout = c("vertical","horizontal"), bounds = FALSE, mid = FALSE, ...)
          {
            switch(layout[1],
                   vertical = plot(as.IntervalList(x), layout = layout, bounds = bounds, mid = mid, xaxt = "n", xlab = "Observation", ...),
                   horizontal = plot(as.IntervalList(x), layout = layout, bounds = bounds, mid = mid, yaxt = "n", ylab = "Observation", ...))
          }
)

setMethod("plot",
          signature(x = "IntervalData", y = "IntervalData"),
          function(x, y, bounds = FALSE, ...)
          {
            plot(as.IntervalList(x), as.IntervalList(y), bounds = bounds, xlab = "First observation", ylab = "Second observation", ...)
          }
)
