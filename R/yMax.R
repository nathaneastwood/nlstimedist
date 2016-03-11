yMax <- function(data, y, ...) {
  data %>%
    dplyr::group_by_(...) %>%
    dplyr::summarise(yMax = max(.$y))
}
