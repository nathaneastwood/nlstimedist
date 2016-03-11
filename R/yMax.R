#' @import dplyr
yMax <- function(data, y, ...) {
  data %>%
    group_by_(...) %>%
    summarise(yMax = max(.$y))
}
