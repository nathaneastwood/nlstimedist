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
  integrate(nth_raw_moment, lower = 0, upper = Inf,
            r = params["r"], c = params["c"], t = params["t"])$value
}
#nth_raw_moment_wrap_maple(lm1)
