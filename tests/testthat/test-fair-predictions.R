

test_that("fair_predictions works for classification", {

  skip_on_cran()

  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })

  data$y <- as.integer(data$y) - 1L
  vars <- c("y", "x", "w", "z")

  expect_setequal(colnames(data), vars)

  fp.nms <- c("yhat_meas", "y_meas", "BN", "train_data", "eval_data", "lmbd_seq",
              "lr", "patience", "relu_eps", "neural_models", "x0", "x1", "model",
              "X", "W", "Z", "Y", "cl", "method", "tune_params", "nboot1",
              "nboot2", "task_type")

  # run fair_predictions
  fp <- fair_predictions(
    data = data, X = "x", Z = "z", W = "w", Y = "y", x0 = 0, x1 = 1,
    BN = c("IE"), lmbd_seq = c(1, 2)
  )

  autoplot(fp, type = "causal")
  autoplot(fp, type = "accuracy")

  expect_type(fp, "list")
  expect_named(fp, fp.nms, ignore.order = TRUE)
})

test_that("fair_predictions works for regression", {

  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })

  data$y <- rnorm(length(data$y))
  vars <- c("y", "x", "w", "z")

  expect_setequal(colnames(data), vars)

  fp.nms <- c("yhat_meas", "y_meas", "BN", "train_data", "eval_data", "lmbd_seq",
              "lr", "patience", "relu_eps", "neural_models", "x0", "x1", "model",
              "X", "W", "Z", "Y", "cl", "method", "tune_params", "nboot1",
              "nboot2", "task_type")

  # run fair_predictions
  fp <- fair_predictions(
    data = data, X = "x", Z = "z", W = "w", Y = "y", x0 = 0, x1 = 1,
    BN = c("IE"), lmbd_seq = c(1, 2)
  )

  autoplot(fp, type = "causal")
  autoplot(fp, type = "accuracy")

  expect_type(fp, "list")
  expect_named(fp, fp.nms, ignore.order = TRUE)
})
