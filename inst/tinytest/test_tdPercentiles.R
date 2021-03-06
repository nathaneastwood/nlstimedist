data <- lobelia[lobelia$Temperature == 12.5, ]
data <- tdData(data, x = "Day", y = "Germination")
model <- timedist(data, x = "Day", y = "propMax", r = 0.1, c = 0.5, t = 10)

single <- round(tdPercentiles(model, n = 0.5), 3)
expect_equal(single, c(`50%` = 12.919), info = "Single percentiles are returned")

mult <- tdPercentiles(model, n = seq(0.1, 0.9, 0.1))
mult <- round(mult, 3)
expect <- structure(
  c(9.159, 10.383, 11.269, 12.073, 12.919, 13.952, 15.517, 18.776, 26.447),
  .Names = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")
)
expect_equal(mult, expect, info = "Multiple percentiles are returned")

