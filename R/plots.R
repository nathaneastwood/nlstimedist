#' Create the data for the plots
#'
#' Augment the data from a model output to be in a form suitable for ggplot
#'
#' @param ... A list of models
augmentMultiple <- function(...) {
  models <- list(...)
  lapply(models,
         function(.) {
           augData <- broom::augment(.)
           colnames(augData) <- c("x", "y", "fitted", "resid")
           augData
         })
}

#' Plot the timedist CDF
#'
#' Given a model (or models) of class \code{timedist}, produce a cumulative distribution plot for
#' each of them.
#'
#' @param ... A model (or a list of models) of class \code{timedist}.
#'
#' @import ggplot2
#' @export
ggtimedistCDF <- function(...) {
  models <- list(...)
  if (length(models) == 1) {
    data <- broom::augment(...)
    colnames(data) <- c("x", "y", "fitted", "resid")
    ggplot(data = data) +
      geom_point(aes(x = "x", y = "y")) +
      geom_line(aes(x = "x", y = "fitted"))
  } else {
    data <- augmentMultiple(...)
    modelNo <- paste0("model", rep(seq_along(data), lapply(data, nrow)))
    data <- do.call("rbind", data)
    data$group <- modelNo
    ggplot(data = data, aes_string(x = "x", y = "y", colour = "group")) +
      geom_point() +
      geom_line(aes_string(x = "x", y = "fitted", colour = "group"))
  }
}

#' Plot the timedist PDF
#'
#' Given a model (or models) of class \code{timedist}, produce a probability density function plot
#' for each of them.
#'
#' @param ... A model (or a list of models) of class \code{timedist}.
#' @param xlim The x limits (x1, x2) of the plot.
#'
#' @import ggplot2
#' @export
ggtimedistPDF <- function(..., xlim = NULL) {
  params <- lapply(list(...), function(.) .$m$getPars())
  data <- augmentMultiple(...)
  modelNo <- paste0("model", seq_along(data))
  data <- do.call("rbind", data)
  if (is.null(xlim)) xlim <- seq(min(data$x), max(data$x), by = max(data$x) / 1000)
  pdfData <- lapply(params, function(.) {
    data.frame(y = tdPDF(xlim, S = 1, .["r"], .["c"], .["t"]),
               x = xlim)
  })
  nRows <- lapply(pdfData, nrow)
  pdfData <- do.call("rbind", pdfData)
  pdfData$group <- rep(modelNo, nRows)
  models <- list(...)
  p <- if (length(models) > 1) {
    ggplot(data = pdfData, aes_string(x = "x", y = "y", colour = "group")) +
      geom_line()
  } else {
    ggplot(data = pdfData, aes_string(x = "x", y = "y")) +
      geom_line()
  }
  p
}
