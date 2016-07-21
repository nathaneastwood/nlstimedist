#' @title Calculate the corrected residual sum of squares
#'
#' @description Calculate the corrected residual sum of squares for a model of
#'   class \code{timedist}.
#'
#' @param model An object of class \code{timedist}.
#'
#' @return A single value.
#'
#' @export
tdRSS <- function(model) {
  1 - sum((as.list(model$m$getEnv())$y - model$m$fitted()) ^ 2) /
    sum((as.list(model$m$getEnv())$y - mean(as.list(model$m$getEnv())$y)) ^ 2)
}
