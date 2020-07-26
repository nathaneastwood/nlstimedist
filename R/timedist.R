#' Prepare `nlstimedist` data
#'
#' The data for `nlstimedist` needs to be in a particular format. This function prepares the data for the model.
#'
#' @param data A `data.frame`. The raw data to be cleaned.
#' @param x `character(1)`. The time variable.
#' @param y `character(1)`. The number of events.
#' @param group `character(1)`. The run numbers. This is `NULL` by default if you are only using the function for one
#' run.
#'
#' @return
#' A `data.frame` of the cleaned data to be supplied to the [timedist()] function.
#'
#' @examples
#' tdData(tilia, x = "Day", y = "Trees")
#' tdData(lobelia, x = "Day", y = "Germination", group = "Temperature")
#'
#' @export
tdData <- function(data, x, y, group = NULL) {

  if (!is.data.frame(data)) stop(paste0("Expecting a `data.frame` but receieved an object of class ", class(data)))
  if (!is.numeric(data[, x])) stop("`x` is not numeric")
  if (!is.numeric(data[, y])) stop("`y` is not numeric")
  isNA <- is.na(data[, y])
  if (any(isNA)) {
    message("Replacing ", sum(isNA), " NAs with `0`")
    data <- poorman::replace_na(data = data, replace = list(y = 0))
  }

  if (!is.null(group)) {
    data <- eval(bquote(poorman::group_by(.data = data, .(as.name(group)))))
  }

  data <- eval(bquote(poorman::filter(.data = data, .(as.name(y)) != 0)))
  data <- eval(bquote(poorman::mutate(
    .data = data,
    "cumN" = cumsum(.(as.name(y))),
    "propMax" = cumN / max(cumN)
  )))

  if (!is.null(group)) data <- poorman::ungroup(data)

  data
}

#' Fit the Franco model
#'
#' Fit the time-course of biological phenomena.
#'
#' @details
#' The [minpack.lm::nlsLM()] function is used instead of the [stats::nls()] function in order to use the
#' Levenberg-Marquardt algorithm which is an extremely robust method of curve-fitting as it is able to switch
#' between Gauss-Newton and gradient descent. This allows it to cope with far-off-optimal starting values. The standard
#' `nls` function does not use Levenberg-Marquardt; it instead uses the Gauss-Newton type, the PORT routines and a
#' partial linear fit.
#'
#' @param data A `data.frame`. The data to be included in the model.
#' @param x,y `character(1)`. The x and y values in the data, where the y values are the proportions.
#' @param r,c,t `numeric(1)`. The starting parameters for the model.
#' @param ... Additional parameters to be passed to [minpack.lm::nlsLM()].
#'
#' @examples
#' tdTilia <- tdData(tilia, x = "Day", y = "Trees")
#' model <- timedist(data = tdTilia, x = "Day", y = "propMax", r = 0.1, c = 0.5, t = 120)
#' model
#'
#' @seealso
#' [tdPDF()], [tdCDF()], [tdRSS()], [glance()], [tdMoments()], [tdPercentiles()]
#'
#' @importFrom minpack.lm nlsLM
#'
#' @export
timedist <- function(data, x, y, r, c, t, ...) {
  if (missing(y)) stop("y is missing")
  if (missing(x)) stop("x is missing")
  if (r <= 0 | r > 1) stop("r must be greater than 0 or less than or equal to 1")
  if (c <= 0) stop("c must be greater than 0")
  if (t < 0) stop("t must be greater than or equal to 0")

  tdFormula <- stats::as.formula(paste0(y, " ~ ", "1 - (1 - (r / (1 + exp(-c * (", x, " - t))))) ^ ", x))
  model <- minpack.lm::nlsLM(tdFormula, data = data, start = list(r = r, c = c, t = t), ...)
  params <- model$m$getPars()

  modMoments <- tdMoments(r = params["r"], c = params["c"], t = params["t"])
  model$m$getMoments <- function() modMoments
  model$m$getVars <- function() c(x = x, y = y)
  model$m$ymax <- function() max(get(y, model$m$getEnv()))
  model$m$rss <- function() tdRSS(model)

  structure(model, class = c("timedist", "nls"))
}
