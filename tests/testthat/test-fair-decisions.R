
test_that("fair_decisions works", {

  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })

  data$y <- as.integer(data$y) - 1L
  data$d <- rbinom(nrow(data), size = 1, prob = 0.5)
  vars <- c("y", "x", "w", "z", "d")

  expect_setequal(colnames(data), vars)

  fd.nms <- c("d_fcb", "delta_fcb", "data", "delta", "po_diff_sign",
              "po_transform",
              "xgb_mod", "xgb_params", "x0", "x1", "model", "X", "W", "Z",
              "Y", "D", "cl", "method", "tune_params", "nboot1", "nboot2")

  # run fair_predictions
  fd <- fair_decisions(
    data = data, X = "x", Z = "z", W = "w", Y = "y", D = "d", x0 = 0, x1 = 1
  )

  autoplot(fd, type = "decision")
  autoplot(fd, type = "delta")
  autoplot(fd, type = "benefit_fairness")

  expect_type(fd, "list")
  expect_named(fd, fd.nms, ignore.order = TRUE)

  fd_preds <- predict(fd, newdata = data[, c("x", "w", "z")], budget = 0.2)
  expect_type(fd_preds, "list")
  autoplot(fd_preds, type = "decision")
  autoplot(fd_preds, type = "delta")
  autoplot(fd_preds, type = "benefit_fairness")
})
