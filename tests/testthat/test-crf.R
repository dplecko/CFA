
test_that("fairness_cookbook works with causal_forest", {

  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })

  vars <- c("y", "x", "w", "z")

  expect_setequal(colnames(data), vars)

  fc.nms <- c("measures", "x0", "x1", "model", "X", "Z", "W", "Y", "cl",
              "eo", "method", "params")


  expect_message(
    with_seed(
      203,
      ran_err <- fairness_cookbook(data, X = "x", Z = "z", Y = "y", W = "w",
                                   x0 = 0, x1 = 1, method = "causal_forest")
    ),
    regexp = "Caught an error in causal_forest,"
  )

  expect_identical(ran_err$method, "medDML")

  data$y <- as.numeric(data$y) - 1L
  # apply cookbook to various data types of Y
  ran <- with_seed(
    202,
    fairness_cookbook(data, X = "x", W = "w", Z = "z", Y = "y",
                      x0 = 0, x1 = 1, method = "causal_forest")
  )

  # both print() and str() throw

  expect_type(ran, "list")
  expect_named(ran, fc.nms, ignore.order = TRUE)

  expect_s3_class(ran, "faircause")
  expect_s3_class(ran[["measures"]], "data.frame")
  expect_identical(ran[["X"]], "x")

  ran.meas <- ran[["measures"]]
  meas.nms <- c("tv", "ctfde", "ctfse", "ett", "ctfie", "te", "nde", "nie",
                "expse_x1", "expse_x0")
  col.nms <- c("value", "boot", "measure", "rep")
  expect_named(ran.meas, col.nms, ignore.order = TRUE)
  expect_setequal(unique(ran.meas$measure), meas.nms)
})

test_that("fairness_cookbook works with causal_forest", {

  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })

  vars <- c("y", "x", "w", "z")

  expect_setequal(colnames(data), vars)

  fc.nms <- c("measures", "x0", "x1", "model", "X", "W", "Z", "Y", "cl",
              "eo", "method", "params")


  expect_message(
    with_seed(
      203,
      ran_err <- fairness_cookbook(data, X = "x", Z = "z", Y = "y", W = "w",
                                   x0 = 0, x1 = 1, method = "causal_forest")
    ),
    regexp = "Caught an error in causal_forest,"
  )

  expect_message(
    with_seed(
      203,
      ran_err <- fairness_cookbook(data, X = "x", Z = "", Y = "y", W = "w",
                                   x0 = 0, x1 = 1, method = "causal_forest")
    ),
    regexp = "Caught an error in causal_forest,"
  )

  expect_identical(ran_err$method, "medDML")

  data$y <- as.numeric(data$y) - 1L
  # apply cookbook to various data types of Y
  ran <- with_seed(
    202,
    fairness_cookbook(data, X = "x", W = "w", Z = "z", Y = "y",
                      x0 = 0, x1 = 1, method = "causal_forest")
  )

  # both print() and str() throw

  expect_type(ran, "list")
  expect_named(ran, fc.nms, ignore.order = TRUE)

  expect_s3_class(ran, "faircause")
  expect_s3_class(ran[["measures"]], "data.frame")
  expect_identical(ran[["X"]], "x")

  ran.meas <- ran[["measures"]]
  meas.nms <- c("tv", "ctfde", "ctfse", "ett", "ctfie", "te", "nde", "nie",
                "expse_x1", "expse_x0")
  col.nms <- c("value", "boot", "measure", "rep")
  expect_named(ran.meas, col.nms, ignore.order = TRUE)
  expect_setequal(unique(ran.meas$measure), meas.nms)
})
