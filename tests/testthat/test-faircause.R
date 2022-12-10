
test_that("fairness_cookbook input types", {

  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })

  vars <- c("y", "x", "w", "z")

  expect_setequal(colnames(data), vars)

  fc.nms <- c("measures", "x0", "x1", "model", "X", "W", "Z", "Y", "cl",
              "eo")

  # apply cookbook to various data types of Y

  ran <- with_seed(
    202,
    fairness_cookbook(data, X = "x", W = "w", Z = "z", Y = "y",
                      x0 = 0, x1 = 1)
  )

  # both print() and str() throw

  expect_type(ran, "list")
  expect_named(ran, fc.nms, ignore.order = TRUE)

  expect_s3_class(ran, "faircause")
  expect_type(ran[["measures"]], "list")
  expect_identical(ran[["X"]], "x")

  ran.meas <- ran[["measures"]]
  meas.nms <- c("TV", "CtfDE", "CtfSE", "ETT", "CtfIE", "TE", "NDE", "NIE",
                "ExpSE_x1", "ExpSE_x0")
  expect_named(ran.meas, meas.nms, ignore.order = TRUE)

  # expect_snapshot_json(tot_var(ran, "train", "y"))
  # expect_snapshot_json(tot_var(ran, "adapt.train", "y"))

})

test_that("fairness_cookbook_eo works", {

  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
    data$y.hat <- runif(100)
  })

  vars <- c("y", "x", "w", "z", "y.hat")

  expect_setequal(colnames(data), vars)

  fc.nms <- c("measures", "x0", "x1", "model", "X", "W", "Z", "Y", "cl",
              "eo")

  ran <- with_seed(
    202,
    fairness_cookbook_eo(data, X = "x", W = "w", Z = "z", Y = "y",
                         Yhat = "y.hat", x0 = 0, x1 = 1, ylvl = 1)
  )

  expect_type(ran, "list")
  expect_named(ran, fc.nms, ignore.order = TRUE)

  expect_s3_class(ran, "faircause")
  expect_type(ran[["measures"]], "list")
  expect_identical(ran[["X"]], "x")

  ran.meas <- ran[["measures"]]
  meas.nms <- c("TV", "CtfDE", "CtfSE", "ETT", "CtfIE", "TE", "NDE", "NIE",
                "ExpSE_x1", "ExpSE_x0")
  expect_named(ran.meas, meas.nms, ignore.order = TRUE)

  expect_true(ran$eo)

  aut.plt <- autoplot(ran, decompose = "xspec")
  announce_snapshot_file(name = "auto_eo")
  expect_snapshot_plot("auto_eo", print(aut.plt))

  expect_snapshot(summary(ran))

})
