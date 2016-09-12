#' Prepare \code{nlstimedist} data
#'
#' The data for \code{nlstimedist} needs to be in a particular format. This
#' function prepares the data for the model.
#'
#' @param data The raw data to be cleaned.
#' @param x The time variable.
#' @param y The number of events.
#' @param group The run numbers. This is \code{NULL} by default if you are only
#'   using the function for one run.
#'
#' @return
#' A list containing
#' \itemize{
#'   \item raw The raw data supplied to the function, i.e. \code{data}.
#'   \item clean item The cleaned data to be supplied to \code{timedist}.
#' }
#'
#' @examples
#' tdTilia <- tdData(tilia, x = "Day", y = "Trees")
#' tdTilia
#'
#' @export
tdData <- function(data, x, y, group = NULL) {

  if (!is.data.frame(data)) {
    stop(paste0("Expecting a data.frame but receieved an object of class ",
                class(data)))
  }

  out <- list(raw = data)

  testNum <- apply(data[, c(x, y)], 2, is.numeric)
  if (FALSE %in% testNum) stop("Data are not numeric")

  # NA values should be replaced with 0s
  isNA <- is.na(data[, y])
  replaceCall <- lazyeval::interp(~ replace(y, isNA, 0), y = as.name(y))
  data <-
    data %>%
    dplyr::mutate_(.dots = stats::setNames(list(replaceCall), y))
  if (sum(isNA) > 0) warning(paste0("Replaced ", sum(isNA), " NAs with 0"))

  # If there are multiple runs, we need to group the data
  if (!is.null(group)) {
    groupDots <- lapply(group, as.symbol)
    data <-
      data %>%
      dplyr::group_by_(.dots = groupDots)
  }

  # Filter out any 0s and calculate the cumulative sum of y and the proporiton
  # for y
  filtZeroCall <- lazyeval::interp(~ y != 0, y = as.name(y))
  cumNCall <- lazyeval::interp(~ cumsum(y), y = as.name(y))
  propMaxCall <- lazyeval::interp(~ cumN / max(cumN), cumN = as.name("cumN"))
  data <-
    data %>%
    dplyr::filter_(.dots = filtZeroCall) %>%
    dplyr::mutate_(.dots = stats::setNames(list(cumNCall), "cumN")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(propMaxCall), "propMax"))

  if (!is.null(group)) data <- data %>% dplyr::ungroup()

  out$clean <- data
  structure(out, class = "td")
}

#' @export
print.td <- function(x, ...) {
  print(dplyr::tbl_df(x$clean))
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
#'   algorithm which is an extremely robust method of curve-fitting as it is
#'   able to switch between Gauss-Newton and gradient descent. This allows it to
#'   cope with far-off-optimal starting values. The standard nls function does
#'   not use Levenberg-Marquardt; it instead uses the Gauss-Newton type, the
#'   PORT routines and a partial linear fit.
#'
#' @examples
#' tdTilia <- tdData(tilia, x = "Day", y = "Trees")
#' model <- timedist(data = tdTilia, x = "Day", y = "propMax", r = 0.1, c = 0.5,
#'                   t = 120)
#' model
#'
#' @export
timedist <- function(data, x, y, r, c, t, ...) {
  if ("td" %in% class(data)) data <- data$clean

  if (missing(y)) stop("y is missing")
  if (missing(x)) stop("x is missing")

  if (r <= 0 | r > 1) {
    stop("r must be greater than 0 or less than or equal to 1")
  }
  if (c <= 0) stop("c must be greater than 0")
  if (t < 0) stop("t must be greater than or equal to 0")

  start <- list(r = r, c = c, t = t)

  tdFormula <- paste0(y,
                      " ~ ",
                      "1 - (1 - (r / (1 + exp(-c * (", x, " - t))))) ^ ", x)

  model <- minpack.lm::nlsLM(stats::as.formula(tdFormula),
                             data = data,
                             start = start, ...)
  params <- model$m$getPars()

  modMoments <- tdMoments(r = params["r"],
                          c = params["c"],
                          t = params["t"])
  model$m$getMoments <- function () modMoments
  model$m$getVars <- function () c(x = x, y = y)
  model$m$ymax <- function() max(get(y, model$m$getEnv()))
  model$m$rss <- function() tdRSS(model)

  structure(model,
            class = c("timedist", "nls"))
}
