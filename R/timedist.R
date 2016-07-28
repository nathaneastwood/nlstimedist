#' Prepare \code{nlstimedist} data
#'
#' The data for \code{nlstimedist} needs to be in a particular format. This
#' function prepares the data for the model.
#'
#' @param data The raw data to be cleaned.
#' @param x The time variable.
#' @param y The number of events.
#' @param runNo The run number.
#'
#' @return
#' A list containing
#' \itemize{
#'   \item raw The raw data supplied to the function, i.e. \code{data}.
#'   \item clean item The cleaned data to be supplied to \code{timedist}.
#' }
#'
#' @export
tdData <- function(data, x, y, runNo = NULL) {

  out <- list(raw = data)

  testNum <- apply(data, 2, is.numeric)
  if (FALSE %in% testNum) stop("Data are not numeric")

  # NA values should be replaced with 0s
  isNA <- is.na(data[, y])
  replaceCall <- lazyeval::interp(~ replace(y, isNA, 0), y = as.name(y))
  data <-
    data %>%
    mutate_(.dots = setNames(list(replaceCall), y))
  if (sum(isNA) > 0) warning(paste0("Replaced ", sum(isNA), " NAs with 0"))

  # If there are multiple runs, we need to group the data
  if (!is.null(runNo)) {
    data <-
      data %>%
      group_by_(runNo)
  }

  # Filter out any 0s and calculate the cumulative sum of y and the proporiton
  # for y
  filtZeroCall <- lazyeval::interp(~ y != 0, y = as.name(y))
  cumNCall <- lazyeval::interp(~ cumsum(y), y = as.name(y))
  data <-
    data %>%
    filter_(.dots = setNames(list(filtZeroCall), y)) %>%
    mutate_(.dots = setNames(list(cumNCall), "cumN")) %>%
    mutate(propMax = cumN / max(cumN))

  if (!is.null(runNo)) data <- data %>% ungroup()

  out$clean <- data
  structure(out, class = "td")
}

#' @export
print.td <- function(x, ...) {
  x$clean %>%
    tbl_df %>%
    print
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
