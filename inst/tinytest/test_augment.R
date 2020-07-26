tdTilia <- tdData(tilia, x = "Day", y = "Trees")
model <- timedist(data = tdTilia, x = "Day", y = "propMax", r = 0.1, c = 0.5, t = 120)
res <- nlstimedist:::augment(model)

expect_equal(colnames(res), c("y", "x", "fitted", "resid"), info = "Check column order")
expect_true(max(res[, "y"]) <= 1, info = "Check max of y")
expect_true(min(res[, "y"]) >= 0, info = "Check min of y")
expect_true(max(res[, "fitted"]) <= 1, info = "Check max of fitted")
expect_true(min(res[, "fitted"]) >= 0, info = "Check min of fitted")
