#' Calculate moments for the fitted `timedist` model
#'
#' Calculate individual model summary statistics or use the wrapper, `tdMoments()`, to calculate all model summary
#' statistics.
#'
#' @param r,c,t `numeric(1)`. Parameters of the Franco distribution.
#' @param ... Additional arguments to be passed to [stats::integrate()].
#'
#' @return
#' For the individual summary statistic functions, a single `numeric`; for `tdMoments()`, a single row `data.frame` of
#' `numerics` containing all of the summary statistics as individual columns.
#'
#' @examples
#' tdMoments(r = 0.1, c = 0.5, t = 120)
#'
#' @importFrom stats integrate
#'
#' @export
tdMoments <- function(r, c, t, ...) {
  data.frame(
    "mean" = tdMean(r = r, c = c, t = t, ...),
    "variance" = tdVariance(r = r, c = c, t = t, ...),
    "sd" = sqrt(tdVariance(r = r, c = c, t = t, ...)),
    "skew" = tdSkew(r = r, c = c, t = t, ...),
    "kurtosis" = tdKurtosis(r = r, c = c, t = t, ...),
    "entropy" = tdEntropy(r = r, c = c, t = t, ...)
  )
}

#' @param upper `numeric(1)`. The upper limit of integration. Defaults to `t * 10`. Can be infinite for all moment
#' functions except for entropy.
#' @examples
#' tdMean(r = 0.1, c = 0.5, t = 120)
#' @rdname tdMoments
#' @export
tdMean <- function(r, c, t, upper = t * 10, ...) {
  stats::integrate(meanWorker, lower = 0, upper = upper, r = r, c = c, t = t, ...)$value
}

#' @examples
#' tdVariance(r = 0.1, c = 0.5, t = 120)
#' @rdname tdMoments
#' @export
tdVariance <- function(r, c, t, upper = t * 10, ...) {
  2 * stats::integrate(varWorker, lower = 0, upper = upper, r = r, c = c, t = t, ...)$value -
    (tdMean(r = r, c = c, t = t, ...) ^ 2)
}

#' @examples
#' tdSkew(r = 0.1, c = 0.5, t = 120)
#' @rdname tdMoments
#' @export
tdSkew <- function(r, c, t, upper = t * 10, ...) {
  o <- omegaWorker(r = r, c = c, t = t, ...)
  integrand <- 3 * stats::integrate(skewWorker, lower = 0, upper = upper, r = r, c = c, t = t, ...)$value
  ((o ^ 3) / (tdMean(r = r, c = c, t = t, ...) ^ 3)) * integrand - o * (3 + o ^ 2)
}

#' @param alternative `logical(1)`. Whether to use the alternative calculation method (`TRUE`) or not (default:
#' `FALSE`).
#' @examples
#' tdKurtosis(r = 0.1, c = 0.5, t = 120)
#' tdKurtosis(r = 0.1, c = 0.5, t = 120, alternative = TRUE)
#' @rdname tdMoments
#' @export
tdKurtosis <- function(r, c, t, upper = t * 10, alternative = FALSE, ...) {
  m <- tdMean(r = r, c = c, t = t, ...)
  o <- omegaWorker(r = r, c = c, t = t, ...)
  integrand <- 4 * stats::integrate(tdFranco, lower = 0, upper = upper, r = r, c = c, t = t, ...)$value
  if (alternative) {
    ex3 <- 3 * stats::integrate(skewWorker, lower = 0, upper = upper, r = r, c = c, t = t, ...)$value
    ((o ^ 4) / (m ^ 4)) * integrand - 4 * ((o ^ 4) / (m ^ 3)) * ex3 + (3 * o ^ 2) * (2 + o ^ 2) - 3
  } else {
    ((o ^ 4) / (m ^ 4)) * integrand - 4 * o * tdSkew(r = r, c = c, t = t, ...) - (o ^ 2) * (6 + o ^ 2) - 3
  }
}

#' @examples
#' tdEntropy(r = 0.1, c = 0.5, t = 120)
#' @rdname tdMoments
#' @export
tdEntropy <- function(r, c, t, upper = t * 10, ...) {
  -stats::integrate(entropyWorker, lower = 0, upper = upper, r = r, c = c, t = t, ...)$value
}

# summary functions ----------------------------------------------------------------------------------------------------
tdFranco <- function(x, r, c, t) (x ^ 3) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
meanWorker <- function(x, r, c, t) 1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x)
varWorker <- function(x, r, c, t) x * (1 - ((1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x)))
skewWorker <- function(x, r, c, t) (x ^ 2) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
omegaWorker <- function(x, r, c, t, ...) tdMean(r = r, c = c, t = t, ...) / sqrt(tdVariance(r = r, c = c, t = t, ...))
entropyWorker <- function(x, r, c, t) {
  (
    -((1 - (r / (1 + exp(-c * (x - t))))) ^ x) * (
      log(1 - (r / (1 + exp(-c * (x - t))))) - (x * r * c * exp(-c * (x - t))) /
      (((1 + exp(-c * (x - t))) ^ 2) * (1 - (r / (1 + exp(-c * (x - t))))))
    )
  ) * (
    log(
      -((1 - (r / (1 + exp(-c * (x - t))))) ^ x) * (
        log(1 - (r / (1 + exp(-c * (x - t))))) - (x * r * c * exp(-c * (x - t))) /
        (((1 + exp(-c * (x - t))) ^ 2) * (1 - (r / (1 + exp(-c * (x - t))))))
      )
    ) /
    log(2)
  )
}
