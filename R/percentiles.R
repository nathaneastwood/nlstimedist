#' Calculate percentiles
#'
#' Calculate the percentiles for a given model output
#'
#' @param model An object of class \code{timedist}.
#' @param n A vector of percentiles to be calculated.
#' @param upper The upper end point of the interval to search.
#' @param ... Additional parameters to be passed to
#'   \code{\link[stats]{uniroot}}.
#'
#' @examples
#' tdTilia <- tdData(tilia, x = "Day", y = "Trees")
#' model <- timedist(data = tdTilia, x = "Day", y = "propMax", r = 0.1, c = 0.5,
#'                   t = 120)
#' model
#' tdPercentiles(model, n = 0.5)
#' tdPercentiles(model, n = seq(0, 0.9, 0.1))
#'
#' @export
tdPercentiles <- function(model, n, upper = model$m$getPars()["t"] * 10, ...) {
  params <- model$m$getPars()
  percentile <- function(x, y, r, c, t) {
    1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x - y
  }
  vals <- if (length(n) > 1) {
    do.call("c",
            lapply(n, function (x) {
              stats::uniroot(percentile, lower = 0, upper = upper, y = x,
                             r = params["r"], c = params["c"],
                             t = params["t"], ...)$root
            }))
  } else {
    stats::uniroot(percentile, lower = 0, upper = upper, y = n, r = params["r"],
                   c = params["c"], t = params["t"], ...)$root
  }
  names(vals) <- paste0(n * 100, "%")
  vals
}
