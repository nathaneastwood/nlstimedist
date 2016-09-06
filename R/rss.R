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
  yParam <- unname(model$m$getVars()["y"])
  yDat <- unlist(as.list(model$m$getEnv())[yParam])
  1 - sum((yDat - model$m$fitted()) ^ 2) / sum((yDat - mean(yDat)) ^ 2)
}
