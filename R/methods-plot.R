
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Plot a single interval or a list of intervals
#'
#' @description
#' S4 methods for function plot.
#' As in the generic plot S3 `graphics' method,
#' these methods plot interval-valued data contained in
#' both \code{IntervalData} and \code{IntervalList} objects.
#'
#' @usage
#' \S4method{plot}{IntervalData,missing}(x, y,
#'      layout = c("vertical", "horizontal"),
#'      bounds = FALSE, mid = FALSE,
#'      \dots)
#' 
#' \S4method{plot}{IntervalData,IntervalData}(x, y, bounds = FALSE, \dots)
#' 
#' \S4method{plot}{IntervalList,missing}(x, y,
#'      layout = c("vertical", "horizontal"),
#'      bounds = FALSE, mid = FALSE,
#'      \dots)
#' 
#' \S4method{plot}{IntervalList,IntervalList}(x, y, bounds = FALSE, \dots)
#'
#' @param x A single interval or a unique list with several intervals stored
#'          as an \code{IntervalData} object or as an \code{IntervalList},
#'          instance respectively.
#' @param y A single interval or a unique list with several intervals stored
#'          as an \code{IntervalData} object or as an \code{IntervalList},
#'          instance respectively.
#' @param layout The axes along which the intervals should be displayed.
#'               Only two alternatives are allowed: \code{vertical} (default)
#'               and \code{horizontal}.
#' @param bounds A single \code{logical} value indicating whether the extremes
#'               of the given intervals should be plotted or not (default).
#' @param mid A single \code{logical} value indicating whether the mid-points
#'            of the given intervals should be plotted or not (default).
#' @param \dots Other graphical parameters.
#' 
#' @details
#' Note that in order to get bidimensional plots with interval-valued data,
#' \code{x} and \code{y} arguments must be of the same class, that is, either
#' both are \code{IntervalData} objects or either both are \code{IntervalList}
#' instances. Moreover, in the second case both lists must have the same number
#' of intervals.
#'
#' @exportMethod plot
#' @docType methods
#' @name plot
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
#' @author José García-García \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Some trivial plot() examples for IntervalData objects
#' i1 <- IntervalData(0, 1)
#' i2 <- IntervalData(2, 3)
#' plot(i1)                             ## Plot only an interval vertically
#' plot(i1, layout = "horizontal")      ## Plot only an interval horizontally
#' plot(i1, bounds = TRUE, mid = TRUE)  ## Add bounds and remark mid-point
#' plot(i1, i2)                         ## Plot an interval on each axis 
#' plot(i1, i2, bounds = TRUE)          ## Add bounds
#'
#' ## Some trivial plot() examples for IntervalList objects
#' list1 <- IntervalList(c(0, 3, 2, 5, 6), c(4, 5, 4, 8, 7))
#' list2 <- IntervalList(c(3, 0, 3, 1, 4), c(7, 4, 6, 2, 6))
#' plot(list1)                            ## Plot an interval list vertically
#' plot(list1, layout = "horizontal")     ## Plot an interval list horizontally
#' plot(list1, bounds = TRUE, mid = TRUE) ## Add bounds and remark mid-points
#' plot(list1, list2)                     ## Plot an interval list on each axis 
#' plot(list1, list2, bounds = TRUE)      ## Add bounds
#'
#' ## Further plot() customizations
#' plot(list1, bounds = TRUE, mid = TRUE,
#'      main = "My one-dimensional interval-valued plot",
#'      col = c("blue", "red"), lwd = 2)
#' plot(list1, list2, bounds = TRUE,
#'      main = "My bidimensional interval-valued plot",
#'      col = "blue", lwd = 2)

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

setMethod("plot",
          signature(x = "IntervalList", y = "missing"),
          function(x, y, layout = c("vertical","horizontal"),
                   bounds = FALSE, mid = FALSE,  ...)
          {
            if (length(bounds) != 1 || ! is.logical(bounds))
              stop("'bounds' argument should be a single logical object")
            
            if (length(mid) != 1 || ! is.logical(mid))
              stop("'mid' argument should be a single logical object")
            
            dotarguments <- list(...)

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

            infs <- x@mid - x@spr
            sups <- x@mid + x@spr
            xcord <- seq_len(length(x))

            if (is.null(dotarguments$ylim) && layout == "vertical")
            {
              miny <- min(infs)
              maxy <- max(sups)
              dotarguments$ylim <- c(miny - 0.05 * (maxy - miny),
                                     maxy + 0.05 * (maxy - miny))
            }
            if (is.null(dotarguments$xlim) && layout == "horizontal")
            {
              minx <- min(infs)
              maxx <- max(sups)
              dotarguments$xlim <- c(minx - 0.05 * (maxx - minx),
                                     maxx + 0.05 * (maxx - minx))
            }

            if (is.null(dotarguments$col))
            {
              dotarguments$col <- "black"
            }

            obsnumber <- c(1, length(x))

            if (layout == "vertical")
            {
              do.call("plot.default",
                      c(list(x = obsnumber, y = rep(0, length(obsnumber)),
                             type = "n"), dotarguments))
              if (bounds == FALSE)
              {
                do.call("segments",
                        c(list(x0 = xcord, y0 = infs, x1 = xcord, y1 = sups),
                          dotarguments))
              } else {
                do.call("arrows",
                        c(list(x0 = xcord, y0 = infs, x1 = xcord, y1 = sups),
                          length = 0.05, angle = 90, code = 3, dotarguments))
              }
              if (mid == TRUE)
              {
                points(xcord, x@mid, pch = 16, col = dotarguments$col)
              }
            }
            else if (layout == "horizontal")
            {
              do.call("plot.default", c(list(x = rep(0, length(obsnumber)),
                                             y = obsnumber, type = "n"),
                                        dotarguments))
              if (bounds == FALSE)
              {
                do.call("segments",
                        c(list(y0 = xcord, x0 = infs, y1 = xcord, x1 = sups),
                          dotarguments))
              } else {
                do.call("arrows",
                        c(list(y0 = xcord, x0 = infs, y1 = xcord, x1 = sups),
                          length = 0.05, angle = 90, code = 3, dotarguments))
              }

              if (mid == TRUE)
              {
                points(x@mid, xcord, pch = 16, col = dotarguments$col)
              }
            }
          }
)

setMethod("plot",
          signature(x = "IntervalList", y = "IntervalList"),
          function(x, y, bounds = FALSE, ...)
          {
            if (length(x) != length(y))
              stop("'x' and 'y' arguments should be of the same length")
            
            if (length(bounds) != 1 || ! is.logical(bounds))
              stop("'bounds' argument should be a logical object")

            dotarguments <- list(...)

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

            infsx <- x@mid - x@spr
            supsx <- x@mid + x@spr
            infsy <- y@mid - y@spr
            supsy <- y@mid + y@spr

            if (is.null(dotarguments$xlim))
            {
              minx <- min(infsx)
              maxx <-max(supsx)
              dotarguments$xlim <- c(minx - 0.05 * (maxx - minx),
                                     maxx + 0.05 * (maxx - minx))
            }
            if (is.null(dotarguments$ylim))
            {
              miny <- min(infsy)
              maxy <-max(supsy)
              dotarguments$ylim <- c(miny - 0.05 * (maxy - miny),
                                     maxy + 0.05 * (maxy - miny))
            }

            if (is.null(dotarguments$col))
            {
              dotarguments$col <- "black"
            }

            do.call("plot.default",
                    c(list(x = 0, y = 0, type = "n"), dotarguments))

            if (bounds == FALSE)
            {
              do.call("segments",
                      c(list(x0 = infsx, y0 = y@mid,
                             x1 = supsx, y1 = y@mid), dotarguments))
              do.call("segments",
                      c(list(x0 = x@mid, y0 = infsy,
                             x1 = x@mid, y1 = supsy), dotarguments))
            } else {
              do.call("arrows",
                      c(list(x0 = infsx, y0 = y@mid,
                             x1 = supsx, y1 = y@mid),
                        length = 0.05, angle = 90, code = 3, dotarguments))
              do.call("arrows",
                      c(list(x0 = x@mid, y0 = infsy,
                             x1 = x@mid, y1 = supsy),
                        length = 0.05, angle = 90, code = 3, dotarguments))
            }

            points(x@mid, y@mid, pch = 16, col = dotarguments$col)

          }
)

setMethod("plot",
          signature(x = "IntervalData", y = "missing"),
          function(x, y, layout = c("vertical","horizontal"),
                   bounds = FALSE, mid = FALSE, ...)
          {
            switch(layout[1],
                   vertical = plot(as.IntervalList(x), layout = layout,
                                   bounds = bounds, mid = mid, xaxt = "n",
                                   xlab = "Observation", ...),
                   horizontal = plot(as.IntervalList(x), layout = layout,
                                     bounds = bounds, mid = mid, yaxt = "n",
                                     ylab = "Observation", ...))
          }
)

setMethod("plot",
          signature(x = "IntervalData", y = "IntervalData"),
          function(x, y, bounds = FALSE, ...)
          {
            plot(as.IntervalList(x), as.IntervalList(y), bounds = bounds,
                 xlab = "First observation", ylab = "Second observation", ...)
          }
)
