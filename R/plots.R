#' Create the data for the plots
#'
#' Augment the data from a model output to be in a form suitable for ggplot
#'
#' @param ... A list of models
#'
#' @importFrom broom augment
augmentMultiple <- function(...) {
  models <- list(...)
  lapply(models,
         function(.) {
           augData <- broom::augment(.)
           nameMod <- .$m$getVars()
           augData <- augData[, c(nameMod, ".fitted", ".resid")]
           colnames(augData) <- c("x", "y", "fitted", "resid")
           augData
         })
}

#' Plot the timedist PDF or CDF
#'
#' Given a model (or models) of class \code{timedist}, produce a cumulative
#' distribution plot for each of them.
#'
#' @param ... A model (or a list of models) of class \code{timedist}.
#' @param S Scaling factor for the PDF.
#' @param xVals A sequence of values between the x limits (x1, x2) of the plot.
#'
#' @examples
#' tdTilia <- tdData(tilia, x = "Day", y = "Trees")
#' model <- timedist(data = tdTilia, x = "Day", y = "propMax", r = 0.1, c = 0.5,
#'                   t = 120)
#' tdCdfPlot(model)
#'
#' @import ggplot2
#' @importFrom broom augment
#' @export
tdCdfPlot <- function(..., S = NULL, xVals = NULL) {

  models <- list(...)
  modelNames <- do.call("c", lapply(substitute(list(...))[-1], deparse))

  # Check for valid values of S
  if (any(S <= 0 | S > 1)) stop("S must be between 0 and 1")
  if (is.null(S)) S <- rep(1, length(models))
  if (length(S) != length(models)) {
    if (length(S) == 1) {
      S <- rep(S, length(models))
    } else {
      stop(paste0("Expecting the length of S to be either 1 or to match the ",
                  "number of models (", length(models), ")."))
    }
  }

  # Extract the data from the models
  if (length(models) == 1) {
    data <- broom::augment(...)
    nameMod <- list(...)[[1]]$m$getVars()
    data <- data[, c(nameMod, ".fitted", ".resid")]
    colnames(data) <- c("x", "y", "fitted", "resid")
    if (S != 1) data$y <- data$y * S
  } else {
    multDat <- augmentMultiple(...)
    if (any(S != 1)) {
      multDatY <- lapply(seq_along(multDat), function(.) {
        if (S[.] != 1) {
          multDat[[.]]$y * S[.]
        }
      })
    }
    data <- do.call("rbind", multDat)
    if (any(S != 1)){
      data$y <- do.call("c", multDatY)
    }
  }

  # Generate values on the x-axis to create y-axis values
  if (is.null(xVals)) {
    xVals <- seq(min(data$x), max(data$x), by = max(data$x) / 1000)
  }

  cdfData <- lapply(
    seq_along(models),
    function (.) {
      params <- models[[.]]$m$getPars()
      data.frame(
        fitted = tdCDF(xVals, S = S[.], params["r"], params["c"], params["t"]),
        x = xVals
      )
    }
  )
  nRowsFit <- lapply(cdfData, nrow)
  cdfData <- do.call("rbind", cdfData)
  p <- if (length(models) > 1) {
    cdfData$group <- rep(modelNames, nRowsFit)
    nRows <- do.call("c", lapply(multDat, nrow))
    data$group <- rep(modelNames, nRows)
    ggplot() +
      geom_point(data = data,
                 aes_string(x = "x", y = "y", colour = "group")) +
      geom_line(data = cdfData,
                aes_string(x = "x", y = "fitted", colour = "group"))
  } else {
    ggplot() +
      geom_point(data = data, aes_string(x = "x", y = "y")) +
      geom_line(data = cdfData, aes_string(x = "x", y = "fitted"))
  }
  p
}

#' Given a model (or models) of class \code{timedist}, produce a probability
#' density function plot for each of them.
#'
#' @examples
#' tdPdfPlot(model)
#'
#' @rdname tdCdfPlot
#' @export
tdPdfPlot <- function(..., S = NULL, xVals = NULL) {

  models <- list(...)
  modelNames <- do.call("c", lapply(substitute(list(...))[-1], deparse))

  # Check for valid values of S
  if (any(S <= 0 | S > 1)) stop("S must be between 0 and 1")
  if (is.null(S)) S <- rep(1, length(models))
  if (length(S) != length(models)) {
    if (length(S) == 1) {
      S <- rep(S, length(models))
    } else {
      stop(paste0("Expecting the length of S to be either 1 or to match the ",
                  "number of models (", length(models), ")."))
    }
  }

  # Extract the data from the models
  if (length(models) == 1) {
    data <- broom::augment(...)
    colnames(data) <- c("x", "y", "fitted", "resid")
  } else {
    multDat <- augmentMultiple(...)
    data <- do.call("rbind", multDat)
  }

  # Generate values on the x-axis to create y-axis values
  if (is.null(xVals)) {
    xVals <- seq(min(data$x), max(data$x), by = max(data$x) / 1000)
  }

  pdfData <- lapply(
    seq_along(models),
    function (.) {
      params <- models[[.]]$m$getPars()
      data.frame(
        y = tdPDF(xVals, S = S[.], params["r"], params["c"], params["t"]),
        x = xVals
      )
    }
  )
  nRows <- lapply(pdfData, nrow)
  pdfData <- do.call("rbind", pdfData)
  pdfData$group <- rep(modelNames, nRows)
  p <- if (length(models) > 1) {
    ggplot(data = pdfData, aes_string(x = "x", y = "y", colour = "group")) +
      geom_line()
  } else {
    ggplot(data = pdfData, aes_string(x = "x", y = "y")) +
      geom_line()
  }

  p
}
