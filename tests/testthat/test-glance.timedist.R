context("Test the glance method: glance.timedist")

test_that("Ensure the glance method is returning expected values", {
  data   <- lobelia[lobelia$Temperature == 12.5, ]
  data   <- tdData(data, x = "Day", y = "Germination")
  model  <- timedist(data, x = "Day", y = "propMax", r = 0.1, c = 0.5, t = 10)
  out    <- glance(model)
  expect <- structure(
    list(sigma = 0x1.e7d8cbce1133ap-5, isConv = TRUE, finTol = 0x1p-26,
         logLik = 0x1.550d5a88be3dcp+4, AIC = -0x1.150d5a88be3dcp+5,
         BIC = -0x1.009a322d5f4f2p+5, deviance = 0x1.3f92a128e2ae8p-5,
         df.residual = 11L, RSS = 0x1.efb75762f359bp-1),
    .Names = c("sigma", "isConv", "finTol", "logLik", "AIC", "BIC", "deviance",
               "df.residual", "RSS"),
    row.names = c(NA, -1L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(out, expect)
})
