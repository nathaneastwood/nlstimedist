context("Test the percentiles function: tdPercentiles")

test_that("Ensure tdPercentiles returns the expected values", {
  data   <- lobelia[lobelia$Temperature == 12.5, ]
  data   <- tdData(data, x = "Day", y = "Germination")
  model  <- timedist(data, x = "Day", y = "propMax", r = 0.1, c = 0.5, t = 10)
  test   <- tdPercentiles(model, n = seq(0.1, 0.9, 0.1))
  expect <- structure(
    c(9.15929492855593, 10.3828132354781, 11.2690584877838, 12.0731281033996,
      12.9185127669697, 13.9522001004869, 15.516803872464, 18.7760238000632,
      26.4466820661236),
    .Names = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%"))
  expect_equal(test, expect)
})
