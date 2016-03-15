#' Calculate the PDF
#'
#' Calculate values of the probability density function.
#'
#' @param x Points at which to calculate the the pdf.
#' @param S Unknown
#' @param r,c,t Parameter values within the model.
#'
#' @return A vector of values from the pdf.
#'
#' @export
tdPDF <- function(x, S = 1, r, c, t) {
  ### Need to find out if S is needed in moments, etc.
  -S * ((1 - (r / (1 + exp(-c * (x - t))))) ^ x) *
    (log(1 - (r / (1 + exp(-c * (x - t))))) -
       (x * r * c * exp(-c * (x - t))) /
       (((1 + exp(-c * (x - t))) ^ 2) * (1 - (r / (1 + exp(-c * (x - t)))))))
}
