#' Construct a single row summary "glance" of a timedist model
#'
#' glance methods always return either a one-row data frame, or NULL
#'
#' @param x An object of class \code{timedist}.
#' @param ... Additional arguments (not used).
#'
#' @return \code{glance} returns one row with the columns
#'   \item{sigma}{the square root of the estimated residual variance}
#'   \item{isConv}{whether the fit successfully converged}
#'   \item{finTol}{the achieved convergence tolerance}
#'   \item{logLik}{the data's log-likelihood under the model}
#'   \item{AIC}{the Akaike Information Criterion}
#'   \item{BIC}{the Bayesian Information Criterion}
#'   \item{deviance}{deviance}
#'   \item{df.residual}{residual degrees of freedom}
#'   \item{RSS}{corrected residual sum of squares}
#'
#' @importFrom broom glance
#'
#' @export
glance.timedist <- function(x, ...) {
  class(x) <- "nls"
  ret <- broom::glance(x)
  ret$RSS <- tdRSS(x)
  ret
}
