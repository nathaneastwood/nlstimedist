#' Take a glance at a model
#'
#' Construct a single row of model summary statistics.
#'
#' @param x An object of class `timedist`.
#'
#' @return `glance()` returns a one row `data.frame` with the columns
#'   \item{sigma}{the square root of the estimated residual variance}
#'   \item{isConv}{whether the fit successfully converged}
#'   \item{finTol}{the achieved convergence tolerance}
#'   \item{logLik}{the data's log-likelihood under the model}
#'   \item{AIC}{the Akaike Information Criterion}
#'   \item{BIC}{the Bayesian Information Criterion}
#'   \item{deviance}{deviance}
#'   \item{df.residual}{residual degrees of freedom}
#'   \item{RSS}{corrected residual sum of squares}
#'   \item{nobs}{the number of observations from the model fit}
#'
#' @examples
#' tdTilia <- tdData(tilia, x = "Day", y = "Trees")
#' model <- timedist(data = tdTilia, x = "Day", y = "propMax", r = 0.1, c = 0.5, t = 120)
#' glance(model)
#'
#' @importFrom stats logLik AIC BIC deviance df.residual nobs
#'
#' @export
glance <- function(x) {
  s <- summary(x)
  data.frame(
    sigma = s$sigma,
    isConv = s$convInfo$isConv,
    finTol = s$convInfo$finTol,
    logLik = as.numeric(stats::logLik(x)),
    AIC = stats::AIC(x),
    BIC = stats::BIC(x),
    deviance = stats::deviance(x),
    df.residual = stats::df.residual(x),
    RSS = tdRSS(x),
    nobs = stats::nobs(x)
  )
}
