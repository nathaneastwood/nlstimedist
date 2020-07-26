#' Extract fitted and residual values for a model
#'
#' @param x A `timedist` model.
#'
#' @importFrom stats fitted residuals
#'
#' @noRd
augment <- function(x) {
  pars <- names(x$m$getPars())
  env <- as.list(x$m$getEnv())
  res <- data.frame(env[!(names(env) %in% pars)], stringsAsFactors = FALSE)
  colnames(res) <- c("y", "x")
  res[, "fitted"] <- stats::fitted(x)
  res[, "resid"] <- stats::residuals(x)
  res
}

#' Create the data for the plots
#'
#' Augment the data from a model output to be in a form suitable for ggplot
#'
#' @param ... A list of models
#'
#' @noRd
augmentMultiple <- function(...) {
  lapply(list(...), augment)
}

#' Plot the timedist PDF or CDF
#'
#' Given a model (or models) of class `timedist`, produce a cumulative distribution plot for each of them.
#'
#' @param ... `timedist` model(s).
#' @param S `numeric(1)`. Scaling factor for the PDF.
#' @param xVals `numeric(n)`. A sequence of values between the x limits (x1, x2) of the plot.
#'
#' @examples
#' tdTilia <- tdData(tilia, x = "Day", y = "Trees")
#' model <- timedist(data = tdTilia, x = "Day", y = "propMax", r = 0.1, c = 0.5, t = 120)
#' tdCdfPlot(model)
#'
#' @import ggplot2
#' @export
# nocov start
tdCdfPlot <- function(..., S = NULL, xVals = NULL) {
  models <- list(...)
  modelNames <- do.call(c, lapply(substitute(list(...))[-1], deparse))

  if (any(S <= 0 | S > 1)) stop("S must be between 0 and 1")
  modLen <- length(models)
  if (is.null(S)) S <- rep(1, modLen)
  if (length(S) != modLen) {
    if (length(S) == 1) {
      S <- rep(S, modLen)
    } else {
      stop(paste0("Expecting the length of S to be either 1 or to match the number of models (", modLen, ")."))
    }
  }

  if (modLen == 1L) {
    data <- augment(...)
    if (S != 1) data$y <- data$y * S
  } else {
    multDat <- augmentMultiple(...)
    if (any(S != 1)) multDatY <- lapply(seq_along(multDat), function(.) if (S[.] != 1) multDat[[.]]$y * S[.])
    data <- do.call(rbind, multDat)
    if (any(S != 1)) data$y <- do.call(c, multDatY)
  }

  # Generate values on the x-axis to create y-axis values
  if (is.null(xVals)) xVals <- seq(min(data$x), max(data$x), by = max(data$x) / 1000)

  cdfData <- lapply(
    seq_along(models),
    function(.) {
      params <- models[[.]]$m$getPars()
      data.frame(fitted = tdCDF(xVals, params["r"], params["c"], params["t"], S = S[.]), x = xVals)
    }
  )
  nRowsFit <- lapply(cdfData, nrow)
  cdfData <- do.call(rbind, cdfData)
  p <- if (modLen > 1L) {
    cdfData$group <- rep(modelNames, as.integer(nRowsFit))
    nRows <- as.integer(lapply(multDat, nrow))
    data$group <- rep(modelNames, nRows)
    ggplot() +
      geom_point(data = data, aes_string(x = "x", y = "y", colour = "group")) +
      geom_line(data = cdfData, aes_string(x = "x", y = "fitted", colour = "group"))
  } else {
    ggplot() +
      geom_point(data = data, aes_string(x = "x", y = "y")) +
      geom_line(data = cdfData, aes_string(x = "x", y = "fitted"))
  }
  p
}

#' Given `timedist` model(s), produce a probability density function plot.
#'
#' @examples
#' tdPdfPlot(model)
#'
#' @rdname tdCdfPlot
#' @export
tdPdfPlot <- function(..., S = NULL, xVals = NULL) {

  models <- list(...)
  modelNames <- do.call(c, lapply(substitute(list(...))[-1], deparse))

  # Check for valid values of S
  modLen <- length(models)
  if (any(S <= 0 | S > 1)) stop("S must be between 0 and 1")
  if (is.null(S)) S <- rep(1, modLen)
  if (length(S) != modLen) {
    if (length(S) == 1L) {
      S <- rep(S, modLen)
    } else {
      stop(paste0("Expecting the length of S to be either 1 or to match the number of models (", modLen, ")."))
    }
  }

  # Extract the data from the models
  data <- if (modLen == 1L) augment(...) else do.call(rbind, augmentMultiple(...))

  # Generate values on the x-axis to create y-axis values
  if (is.null(xVals)) xVals <- seq(min(data$x), max(data$x), by = max(data$x) / 1000)

  pdfData <- lapply(
    seq_along(models),
    function(.) {
      params <- models[[.]]$m$getPars()
      data.frame(y = tdPDF(xVals, params["r"], params["c"], params["t"], S = S[.]), x = xVals)
    }
  )
  nRows <- as.integer(lapply(pdfData, nrow))
  pdfData <- do.call(rbind, pdfData)
  pdfData$group <- rep(modelNames, nRows)
  p <- if (modLen > 1L) {
    ggplot(data = pdfData, aes_string(x = "x", y = "y", colour = "group")) +
      geom_line()
  } else {
    ggplot(data = pdfData, aes_string(x = "x", y = "y")) +
      geom_line()
  }

  p
}
# nocov end
