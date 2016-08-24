#' @title Calculate moments for the fitted timedist model
#'
#' @description Individual functions are provided as well as a wrapper to
#' calculate the moments for your fitted model.
#'
#' @param r,c,t Parameters of the Franco distribution
#' @param ... Additional arguments to be passed to
#'   \code{\link[stats]{integrate}}
#'
#' @return A single value, or in the case of \code{tdMoments}, a
#'   \code{data.frame} of values.
#'
#' @export
tdMoments <- function(r, c, t, ...) {
  data.frame("mean" = tdMean(r = r, c = c, t = t, ...),
             "variance" = tdVariance(r = r, c = c, t = t, ...),
             "sd" = sqrt(tdVariance(r = r, c = c, t = t, ...)),
             "skew" = tdSkew(r = r, c = c, t = t, ...),
             "kurtosis" = tdKurtosis(r = r, c = c, t = t, ...),
             "entropy" = tdEntropy(r = r, c = c, t = t, ...))
}

#' @param upper The upper limit of integration. Defaults to \code{t * 10}. Can
#'   be infinite for all moment functions except for entropy.
#' @rdname tdMoments
#' @export
tdMean <- function(r, c, t, upper = t * 10, ...) {
  vars <- list(r = r, c = c, t = t)
  assertr::verify(vars, r > 0)
  assertr::verify(vars, r <= 1)
  assertr::verify(vars, c > 0)
  assertr::verify(vars, t >= 0)
  meanFn <- function(x, r, c, t) {
    1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x)
  }
  stats::integrate(meanFn, lower = 0, upper = upper,
                   r = r, c = c, t = t, ...)$value
}

#' @rdname tdMoments
#' @export
tdVariance <- function(r, c, t, upper = t * 10, ...) {
  vars <- list(r = r, c = c, t = t)
  assertr::verify(vars, r > 0)
  assertr::verify(vars, r <= 1)
  assertr::verify(vars, c > 0)
  assertr::verify(vars, t >= 0)
  varFn <- function(x, r, c, t) {
    x * (1 - ((1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x)))
  }
  2 * stats::integrate(varFn, lower = 0, upper = upper,
                       r = r, c = c, t = t, ...)$value -
    (tdMean(r = r, c = c, t = t, ...) ^ 2)
}

#' @rdname tdMoments
#' @export
tdSkew <- function(r, c, t, upper = t * 10, ...) {
  vars <- list(r = r, c = c, t = t)
  assertr::verify(vars, r > 0)
  assertr::verify(vars, r <= 1)
  assertr::verify(vars, c > 0)
  assertr::verify(vars, t >= 0)
  omega <- tdMean(r = r, c = c, t = t, ...) /
    sqrt(tdVariance(r = r, c = c, t = t, ...))
  mmean <- tdMean(r = r, c = c, t = t, ...)
  skewFn <- function(x, r, c, t) {
    (x ^ 2) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
  }
  integrand <- 3 * stats::integrate(skewFn, lower = 0, upper = upper,
                                    r = r, c = c, t = t,  ...)$value
  ((omega ^ 3) / (mmean ^ 3)) * integrand - omega * (3 + omega ^ 2)
}

#' @param alternative An alternative calculation method.
#' @rdname tdMoments
#' @export
tdKurtosis <- function(r, c, t, upper = t * 10, alternative = FALSE, ...) {
  vars <- list(r = r, c = c, t = t)
  assertr::verify(vars, r > 0)
  assertr::verify(vars, r <= 1)
  assertr::verify(vars, c > 0)
  assertr::verify(vars, t >= 0)
  omega <- tdMean(r = r, c = c, t = t, ...) /
    sqrt(tdVariance(r = r, c = c, t = t, ...))
  mmean <- tdMean(r = r, c = c, t = t, ...)
  mskew <- tdSkew(r = r, c = c, t = t, ...)
  kurt_franco <- function(x, r, c, t) {
    (x ^ 3) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
  }
  integrand <- 4 * stats::integrate(kurt_franco, lower = 0, upper = upper,
                                    r = r, c = c, t = t,  ...)$value
  if (alternative) {
    skewFn <- function(x, r, c, t) {
      (x ^ 2) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
    }
    ex3 <- 3 * stats::integrate(skewFn, lower = 0, upper = upper,
                                r = r, c = c, t = t,  ...)$value
    ((omega ^ 4) / (mmean ^ 4)) * integrand - 4 * ((omega ^ 4) / (mmean ^ 3)) *
      ex3 + (3 * omega ^ 2) * (2 + omega ^ 2) - 3
  } else {
    ((omega ^ 4) / (mmean ^ 4)) * integrand - 4 * omega * mskew - (omega ^ 2) *
      (6 + omega ^ 2) - 3
  }
}

#' @rdname tdMoments
#' @export
tdEntropy <- function(r, c, t, upper = t * 10, ...) {
  vars <- list(r = r, c = c, t = t)
  assertr::verify(vars, r > 0)
  assertr::verify(vars, r <= 1)
  assertr::verify(vars, c > 0)
  assertr::verify(vars, t >= 0)
  entFn <- function(x, r, c, t) {
    (-((1 - (r / (1 + exp(-c * (x - t))))) ^ x) *
       (log(1 - (r / (1 + exp(-c * (x - t))))) -
          (x * r * c * exp(-c * (x - t))) /
          (((1 + exp(-c * (x - t))) ^ 2) *
             (1 - (r / (1 + exp(-c * (x - t)))))))) *
      (log(-((1 - (r / (1 + exp(-c * (x - t))))) ^ x) *
             (log(1 - (r / (1 + exp(-c * (x - t))))) -
                (x * r * c * exp(-c * (x - t))) /
                (((1 + exp(-c * (x - t))) ^ 2) *
                   (1 - (r / (1 + exp(-c * (x - t)))))))) / log(2))
  }
  -stats::integrate(entFn, lower = 0, upper = upper,
                    r = r, c = c, t = t, ...)$value
}
