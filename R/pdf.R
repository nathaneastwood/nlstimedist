#' Calculate the PDF and CDF
#'
#' Calculate values of the probability density function (PDF) and the cumulative distribution function (CDF).
#'
#' @param x `numeric(n)`. Points at which to calculate the the PDF.
#' @param r,c,t `numeric(1)`. Parameter values within the model.
#' @param S `numeric(1)`. Scaling factor for the PDF.
#'
#' @return A vector of values from the PDF or CDF.
#'
#' @seealso
#' [tdPdfPlot()], [tdCdfPlot()]
#'
#' @export
tdPDF <- function(x, r, c, t, S = 1) {
  -S * ((1 - (r / (1 + exp(-c * (x - t))))) ^ x) * (
    log(1 - (r / (1 + exp(-c * (x - t))))) - (x * r * c * exp(-c * (x - t))) /
      (((1 + exp(-c * (x - t))) ^ 2) * (1 - (r / (1 + exp(-c * (x - t))))))
  )
}

#' @rdname tdPDF
#' @export
tdCDF <- function(x, r, c, t, S = 1) {
  S * (1 - (1 - (r / (1 + exp(-c * (x - t))))) ^ x)
}
