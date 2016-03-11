timedistMoments <- function(object, ...) {
  data.frame("mean" = timedistMean(object, ...),
             "variance" = timedistVariance(object, ...),
             "sd" = sqrt(timedistVariance(object, ...)),
             "skew" = timedistSkew(object, ...),
             "kurtosis" = timedistKurtosis(object, ...),
             "entropy" = timedistEntropy(object, ...))
}

# Percentiles
# Need to define upper
timedistPercentiles <- function(object, n, upper = object$m$getPars()["t"] * 10, ...) {
  params <- object$m$getPars()
  percentile <- function(x, y, r, c, t) {
    1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x - y
  }
  uniroot(percentile, lower = 0, upper = Inf, y = n, r = params["r"],
          c = params["c"], t = params["t"], ...)$root
}
#franco_percentiles(lm1, n = 0.99)

# Create a new class, then object will be of class "new class"
nth_raw_moment_wrap <- function(object, n, ...) {

  params <- object$m$getPars()

  nth_raw_moment <- function(x, n, r, c, t) {
    x ^ (n - 1) * (1 - (r / (1 + exp(-c * (x - t))))) ^ x
  }
  n * integrate(nth_raw_moment, lower = 0, upper = Inf, n = n, r = params["r"], c = params["c"], t = params["t"])$value
}
#nth_raw_moment_wrap(lm1, 0.99)

# Percentile
nth_raw_moment_wrap_maple <- function(object, ...) {

  params <- object$m$getPars()

  nth_raw_moment <- function(x, r, c, t) {
    1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x
  }
  integrate(nth_raw_moment, lower = 0, upper = Inf, r = params["r"], c = params["c"], t = params["t"])$value
}
#nth_raw_moment_wrap_maple(lm1)

# Mean
# ... additional parameters to be passed to \code{\link[stats]{integrate}}
timedistMean <- function(object, ...) {
  params <- object$m$getPars()
  meanFn <- function(x, r, c, t) {
    1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x)
  }
  integrate(meanFn, lower = 0, upper = Inf, r = params["r"], c = params["c"], t = params["t"], ...)$value
}
#timedistMean(lm1)

# Variance
timedistVariance <- function(object, ...) {
  params <- object$m$getPars()
  varFn <- function(x, r, c, t) {
    x * (1 - ((1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x)))
  }
  2 * integrate(varFn, lower = 0, upper = Inf, r = params["r"], c = params["c"], t = params["t"], ...)$value -
    (timedistMean(object, ...) ^ 2)
}
#timedistVariance(lm1)

# Standard deviation
#sqrt(timedistVariance(object))

# Skewness
timedistSkew <- function(object, ...) {
  params <- object$m$getPars()
  omega <- timedistMean(object, ...) / sqrt(timedistVariance(object, ...))
  mmean <- timedistMean(object, ...)
  skewFn <- function(x, r, c, t) {
    (x ^ 2) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
  }
  integrand <- 3 * integrate(skewFn, lower = 0, upper = Inf, r = params["r"], c = params["c"], t = params["t"], ...)$value
  ((omega ^ 3) / (mmean ^ 3)) * integrand - omega * (3 + omega ^ 2)
}
#timedistSkew(lm1)

# Excess Kurtosis
timedistKurtosis <- function(object, alternative = FALSE, ...) {
  params <- object$m$getPars()
  omega <- timedistMean(object, ...) / sqrt(timedistVariance(object, ...))
  mmean <- timedistMean(object, ...)
  mskew <- timedistSkew(object, ...)
  kurt_franco <- function(x, r, c, t) {
    (x ^ 3) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
  }
  integrand <- 4 * integrate(kurt_franco, lower = 0, upper = Inf, r = params["r"], c = params["c"], t = params["t"], ...)$value
  if (alternative) {
    skewFn <- function(x, r, c, t) {
      (x ^ 2) * (1 - (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x))
    }
    ex3 <- 3 * integrate(skewFn, lower = 0, upper = Inf, r = params["r"], c = params["c"], t = params["t"], ...)$value
    ((omega ^ 4) / (mmean ^ 4)) * integrand - 4 * ((omega ^ 4) / (mmean ^ 3)) * ex3 + (3 * omega ^ 2) * (2 + omega ^ 2) - 3
  } else {
    ((omega ^ 4) / (mmean ^ 4)) * integrand - 4 * omega * mskew - (omega ^ 2) * (6 + omega ^ 2) - 3
  }
}
#timedistKurtosis(lm1)
#timedistKurtosis(lm1, alternative = TRUE)

# Entropy - not working when upper = Inf as
# > entFn(Inf, params["r"], params["c"], params["t"])
# NaN
timedistEntropy <- function(object, ...) {
  params <- object$m$getPars()
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
  t <- params["t"]
  integrand <- -integrate(entFn, lower = 0, upper = t * 10, r = params["r"], c = params["c"], t = t, ...)$value
  integrand
}
#timedistEntropy(lm1)
