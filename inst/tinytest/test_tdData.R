out <- tdData(lobelia, x = "Day", y = "Germination", group = "Temperature")
expect_equal(
  colnames(out),
  c("Day", "Temperature", "Germination", "cumN", "propMax"),
  info = "Ensure cumN and propMax are calculated and returned"
)
expect_true(inherits(out, "data.frame"), info = "Ensure a data.frame is returned")
expect_true(nrow(out[out$Germination == 0, ]) == 0L, info = "Ensure 0s are removed from the y row")
expect_equal(
  out$cumN,
  c(
    3L, 6L, 7L, 8L, 9L, 11L, 12L, 14L, 16L, 1L, 2L, 8L, 10L, 11L,  14L, 15L, 16L, 18L, 19L, 20L, 21L, 22L, 23L, 1L, 3L,
    7L, 10L, 12L, 16L, 23L, 26L, 28L, 31L, 33L, 35L, 36L, 37L, 38L, 39L, 40L, 8L, 16L, 23L, 32L, 37L, 40L, 49L, 51L,
    52L, 53L, 1L, 11L, 28L, 38L, 42L, 47L, 52L, 55L, 60L, 63L, 64L, 65L, 1L, 13L, 26L, 40L, 46L, 55L, 56L, 64L, 65L,
    67L, 68L, 10L, 26L, 34L, 46L, 48L, 51L, 58L, 62L, 63L
  ),
  info = "Ensure the grouping variable is taken into consideration"
)
expect_error(tdData(as.list(lobelia), x = "Day", y = "Trees"), info = "A data.frame must be given")
