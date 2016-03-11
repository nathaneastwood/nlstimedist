#' @title Fit the Franco model
#'
#' @description Fit the Franco model
#'
#' @param data The data to be included in the model.
#' @param start A list of starting parameters for \code{r}, \code{c} and \code{t}.
#' @param ... Additional parameters to be passed to \code{\link[minpack.lm]{nlsLM}}.
#'
#' @export
timedist <- function(data, start, ...) {

  assertr::verify(start, r > 0)
  assertr::verify(start, r <= 1)
  assertr::verify(start, c > 0)
  assertr::verify(start, t >= 0)
  # This function needs to:
  # remove any zeros? - need to find out!

  model <- minpack.lm::nlsLM(y ~ 1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x,
                             data = data,
                             start = start, ...)
  model$m$moments <- timedistMoments(model)
  model$m$rss <- timedistRSS(model)

  structure(model,
            class = c("nls", "timedist"))
}
