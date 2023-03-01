
test_that("Z, W sets empty", {

  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })
  data$y <- as.integer(data$y) - 1L

  for (method in c("medDML", "causal_forest")) {

    # test empty Z and W sets
    ran_z <- with_seed(
      202,
      fairness_cookbook(data, X = "x", W = "w", Z = "", Y = "y",
                        x0 = 0, x1 = 1, method = method)
    )

    meas <- summary(ran_z)$measures

    expect_equal(meas[meas$measure == "ctfse", ]$value, 0)
    expect_equal(meas[meas$measure == "expse_x1", ]$value, 0)
    expect_equal(meas[meas$measure == "expse_x0", ]$value, 0)

    expect_equal(meas[meas$measure == "te", ]$value,
                 meas[meas$measure == "ett", ]$value)
    expect_equal(meas[meas$measure == "te", ]$value,
                 meas[meas$measure == "tv", ]$value)

    ran_w <- with_seed(
      202,
      fairness_cookbook(data, X = "x", W = "", Z = "z", Y = "y",
                        x0 = 0, x1 = 1, method = method)
    )

    meas <- summary(ran_w)$measures

    expect_equal(meas[meas$measure == "ctfie", ]$value, 0)
    expect_equal(meas[meas$measure == "nie", ]$value, 0)

    expect_equal(meas[meas$measure == "te", ]$value,
                 meas[meas$measure == "nde", ]$value)
    expect_equal(meas[meas$measure == "ett", ]$value,
                 meas[meas$measure == "ctfde", ]$value)

    ran_zw <- with_seed(
      202,
      fairness_cookbook(data, X = "x", W = "", Z = "", Y = "y",
                        x0 = 0, x1 = 1, method = method)
    )

    meas <- summary(ran_zw)$measures

    expect_equal(meas[meas$measure == "ctfse", ]$value, 0)
    expect_equal(meas[meas$measure == "expse_x1", ]$value, 0)
    expect_equal(meas[meas$measure == "expse_x0", ]$value, 0)

    expect_equal(meas[meas$measure == "te", ]$value,
                 meas[meas$measure == "ett", ]$value)
    expect_equal(meas[meas$measure == "te", ]$value,
                 meas[meas$measure == "tv", ]$value)

    expect_equal(meas[meas$measure == "ctfie", ]$value, 0)
    expect_equal(meas[meas$measure == "nie", ]$value, 0)

    expect_equal(meas[meas$measure == "te", ]$value,
                 meas[meas$measure == "nde", ]$value)
    expect_equal(meas[meas$measure == "ett", ]$value,
                 meas[meas$measure == "ctfde", ]$value)
  }

})
