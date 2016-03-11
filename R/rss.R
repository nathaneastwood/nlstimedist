#' @export
timedistRSS <- function(model) {
  1 - sum((as.list(model$m$getEnv())$y - model$m$fitted()) ^ 2) /
    sum((as.list(model$m$getEnv())$y - mean(as.list(model$m$getEnv())$y)) ^ 2)
}
