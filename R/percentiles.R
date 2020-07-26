#' Calculate percentiles
#'
#' Calculate the percentiles for a given model.
#'
#' @param model An object of class `timedist`.
#' @param n `numeric(n)`. A vector of percentiles to be calculated.
#' @param upper `numeric(1)`. The upper end point of the interval to search.
#' @param ... Additional parameters to be passed to [stats::uniroot()].
#'
#' @examples
#' tdTilia <- tdData(tilia, x = "Day", y = "Trees")
#' model <- timedist(data = tdTilia, x = "Day", y = "propMax", r = 0.1, c = 0.5, t = 120)
#' tdPercentiles(model, n = 0.5)
#' tdPercentiles(model, n = seq(0, 0.9, 0.1))
#'
#' @importFrom stats uniroot
#'
#' @export
tdPercentiles <- function(model, n, upper = model$m$getPars()["t"] * 10, ...) {
  params <- model$m$getPars()
  vals <- if (length(n) > 1L) {
    do.call(c, lapply(n, function (x) {
      stats::uniroot(
        percentile, lower = 0, upper = upper, y = x, r = params["r"], c = params["c"], t = params["t"], ...
      )$root
    }))
  } else {
    stats::uniroot(
      percentile, lower = 0, upper = upper, y = n, r = params["r"], c = params["c"], t = params["t"], ...
    )$root
  }
  names(vals) <- paste0(n * 100, "%")
  vals
}

# helpers --------------------------------------------------------------------------------------------------------------
percentile <- function(x, y, r, c, t) 1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x - y
