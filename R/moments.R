#' @title Calculate moments for the fitted td model
#'
#' @description Individual functions are provided as well as a wrapper to calculate the moments for
#' your fitted model.
#'
#' @param object A model of class \code{td}.
#' @param ... Additional arguments to be passed to \code{\link[stats]{integrate}}
#'
#' @return A single value, or in the case of \code{tdMoments}, a \code{data.frame} of values.
#'
#' @export
tdMoments <- function(object, ...) {
  data.frame("mean" = tdMean(object, ...),
             "variance" = tdVariance(object, ...),
             "sd" = sqrt(tdVariance(object, ...)),
             "skew" = tdSkew(object, ...),
             "kurtosis" = tdKurtosis(object, ...),
             "entropy" = tdEntropy(object, ...))
}

#' @param upper Defaults to \code{Inf} for all moment functions except for entropy which uses
#'   \code{t * 10} by default.
#' @rdname tdMoments
#' @export
tdMean <- function(object, upper = Inf, ...) {
  params <- object$m$getPars()
  meanFn <- function(x, r, c, t) {
    1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x)
  }
  integrate(meanFn, lower = 0, upper = upper,
            r = params["r"], c = params["c"], t = params["t"], ...)$value
}

#' @rdname tdMoments
#' @export
tdVariance <- function(object, upper = Inf, ...) {
  params <- object$m$getPars()
  varFn <- function(x, r, c, t) {
    x * (1 - ((1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x)))
  }
  2 * integrate(varFn, lower = 0, upper = upper,
                r = params["r"], c = params["c"], t = params["t"], ...)$value -
    (tdMean(object, ...) ^ 2)
}

#' @rdname tdMoments
#' @export
tdSkew <- function(object, upper = Inf, ...) {
  params <- object$m$getPars()
  omega <- tdMean(object, ...) / sqrt(tdVariance(object, ...))
  mmean <- tdMean(object, ...)
  skewFn <- function(x, r, c, t) {
    (x ^ 2) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
  }
  integrand <- 3 * integrate(skewFn, lower = 0, upper = upper,
                             r = params["r"], c = params["c"], t = params["t"], ...)$value
  ((omega ^ 3) / (mmean ^ 3)) * integrand - omega * (3 + omega ^ 2)
}

#' @param alternative An alternative calculation method
#' @rdname tdMoments
#' @export
tdKurtosis <- function(object, upper = Inf, alternative = FALSE, ...) {
  params <- object$m$getPars()
  omega <- tdMean(object, ...) / sqrt(tdVariance(object, ...))
  mmean <- tdMean(object, ...)
  mskew <- tdSkew(object, ...)
  kurt_franco <- function(x, r, c, t) {
    (x ^ 3) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
  }
  integrand <- 4 * integrate(kurt_franco, lower = 0, upper = upper,
                             r = params["r"], c = params["c"], t = params["t"], ...)$value
  if (alternative) {
    skewFn <- function(x, r, c, t) {
      (x ^ 2) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
    }
    ex3 <- 3 * integrate(skewFn, lower = 0, upper = upper,
                         r = params["r"], c = params["c"], t = params["t"], ...)$value
    ((omega ^ 4) / (mmean ^ 4)) * integrand - 4 * ((omega ^ 4) / (mmean ^ 3)) *
      ex3 + (3 * omega ^ 2) * (2 + omega ^ 2) - 3
  } else {
    ((omega ^ 4) / (mmean ^ 4)) * integrand - 4 * omega * mskew - (omega ^ 2) * (6 + omega ^ 2) - 3
  }
}

#' @rdname tdMoments
#' @export
tdEntropy <- function(object, upper = NULL, ...) {
  params <- object$m$getPars()
  if (is.null(upper)) upper <- params["t"] * 10
  entFn <- function(x, r, c, t) {

    (-((1 - (r / (1 + exp(-c * (x - t))))) ^ x) *
       (log(1 - (r / (1 + exp(-c * (x - t))))) -
          (x * r * c * exp(-c * (x - t))) /
          (((1 + exp(-c * (x - t))) ^ 2) * (1 - (r / (1 + exp(-c * (x - t)))))))) *
      (log(-((1 - (r / (1 + exp(-c * (x - t))))) ^ x) *
             (log(1 - (r / (1 + exp(-c * (x - t))))) -
                (x * r * c * exp(-c * (x - t))) /
                (((1 + exp(-c * (x - t))) ^ 2) * (1 - (r / (1 + exp(-c * (x - t)))))))) / log(2))
  }
  integrand <- -integrate(entFn, lower = 0, upper = upper,
                          r = params["r"], c = params["c"], t = params["t"], ...)$value
  integrand
}
