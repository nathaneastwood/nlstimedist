#' @title Fit the Franco model
#'
#' @description Fit the Franco model
#'
#' @param data The data to be included in the model.
#' @param x,y The x and y values in the data.
#' @param r,c,t The starting parameters for the model.
#' @param ... Additional parameters to be passed to \code{\link[minpack.lm]{nlsLM}}.
#'
#' @details The \code{\link[minpack.lm]{nlsLM}} function is used instead of the
#'   \code{\link[stats]{nls}} function in order to use the Levenberg-Marquardt algorithm because...
#'
#' @export
timedist <- function(data, x, y, r, c, t, ...) {
  # timedist <- function(data, x, y, start, ...) {
  # timedist <- function(data, x, y, r, c, t, ...) {                             <- this is probably the easiest way. Then in the model, I would have start = list(r = r, c = c, ...)
  # With these above two methods, I would need additional checks to ensure that x and y are pointing to the correct things in the data.
  ############################################
  ### Need to think about global variable bindings for x, y, r, c and t.
  ### Maybe think about all.vars(as.formula(...))
  ############################################

  start <- list(r = r, c = c, t = t)

  assertr::verify(start, r > 0)
  assertr::verify(start, r <= 1)
  assertr::verify(start, c > 0)
  assertr::verify(start, t >= 0)
  # This function needs to:
  # remove any zeros? - need to find out!

  model <- minpack.lm::nlsLM(y ~ 1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x,
                             data = data,
                             start = start, ...)
  params <- model$m$getPars()
  model$m$moments <- tdMoments(r = params["r"], c = params["c"], t = params["t"])
  model$m$ymax <- max(model$m$getEnv()$y)
  model$m$rss <- tdRSS(model)

  structure(model,
            class = c("timedist", "nls"))
}
