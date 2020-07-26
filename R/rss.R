#' Calculate the corrected residual sum of squares
#'
#' Calculate the corrected residual sum of squares for a `timedist` model.
#'
#' @param model An object of class `timedist`.
#'
#' @return `numeric(1)`.
#'
#' @examples
#' tdTilia <- tdData(tilia, x = "Day", y = "Trees")
#' model <- timedist(data = tdTilia, x = "Day", y = "propMax", r = 0.1, c = 0.5, t = 120)
#' model
#' tdRSS(model)
#'
#' @export
tdRSS <- function(model) {
  yParam <- unname(model$m$getVars()["y"])
  yDat <- unlist(as.list(model$m$getEnv())[yParam])
  1 - sum((yDat - model$m$fitted()) ^ 2) / sum((yDat - mean(yDat)) ^ 2)
}
