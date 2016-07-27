#' Prepare \code{nlstimedist} data
#'
#' The data for \code{nlstimedist} needs to be in a particular format. This
#' function prepares the data for the model.
#'
#' @param data The raw data to be cleaned.
#' @param x The time variable.
#' @param y The number of events.
#'
#' @return
#' A list containing
#' \itemize{
#'   \item raw The raw data supplied to the function, i.e. \code{data}.
#'   \item clean item The cleaned data to be supplied to \code{timedist}.
#' }
#'
#' @export
tdData <- function(data, x, ...) {

  testNum <- apply(data[, c(x, ...)], 2, is.numeric)
  if (FALSE %in% testNum) stop("Data are not numeric")

  ys <- list(...)
  cleans <- lapply(ys,
         function(.) {
           subData <- data[, c(x, .)]
           if (any(is.na(subData[, .]))) {
             isNA <- is.na(subData[, .])
             subData <- subData[!isNA, ]
             warning(paste0("Replaced ", sum(isNA), " NAs with 0"))
           }
           if (any(subData[, .] == 0)) {
             rowsWithoutZero <- which(subData[, .] == 0)
             clean <- if (length(rowsWithoutZero) == 1 &&
                          rowsWithoutZero == 1) {
               subData[-1, ]
             } else {
               subData[-rowsWithoutZero[rowsWithoutZero != 1], ]
             }
           }
           clean$cumN <- cumsum(clean[, .])
           clean$propMax <- clean$cumN / max(clean$cumN)
           rownames(clean) <- NULL
           clean
         })
  names(cleans) <- paste0("clean_", c(...))

  structure(list(raw = data,
                 clean = cleans),
            class = "td")
}

#' @export
print.td <- function(x, ...) {
  str(x$clean)
}

#' @title Fit the Franco model
#'
#' @description Fit the Franco model
#'
#' @param data The data to be included in the model.
#' @param x,y The x and y values in the data, where the y values are the
#'   proportions.
#' @param r,c,t The starting parameters for the model.
#' @param ... Additional parameters to be passed to
#'   \code{\link[minpack.lm]{nlsLM}}.
#'
#' @details The \code{\link[minpack.lm]{nlsLM}} function is used instead of the
#'   \code{\link[stats]{nls}} function in order to use the Levenberg-Marquardt
#'   algorithm because...
#'
#' @export
timedist <- function(data, x, y, r, c, t, ...) {

  if (missing(y)) stop("y is missing")
  if (missing(x)) stop("x is missing")

  start <- list(r = r, c = c, t = t)

  assertr::verify(start, r > 0)
  assertr::verify(start, r <= 1)
  assertr::verify(start, c > 0)
  assertr::verify(start, t >= 0)

  tdFormula <- paste0(y,
                      " ~ ",
                      "1 - (1 - (r / (1 + exp(-c * (", x, " - t))))) ^ ", x)

  model <- minpack.lm::nlsLM(as.formula(tdFormula),
                             data = data,
                             start = start, ...)
  params <- model$m$getPars()
  model$m$moments <- tdMoments(r = params["r"],
                               c = params["c"],
                               t = params["t"])
  model$m$ymax <- max(get(y, model$m$getEnv()))
  model$m$rss <- tdRSS(model)

  structure(model,
            class = c("timedist", "nls"))
}
