timedistPDF <- pdf <- function(x, S = 1, r, c, t) {
  -S * ((1 - (r / (1 + exp(-c * (x - t))))) ^ x) *
    (log(1 - (r / (1 + exp(-c * (x - t))))) -
       (x * r * c * exp(-c * (x - t))) /
       (((1 + exp(-c * (x - t))) ^ 2) * (1 - (r / (1 + exp(-c * (x - t)))))))
}
