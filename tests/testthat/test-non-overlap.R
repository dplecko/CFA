
test_that("fairness_cookbook reports non-overlap", {

  nsamp <- 500
  data <- with_seed(
    203, ex_med_bias(nsamp)
  )

  gtruth <- with_seed(203, ex_med_bias(nsamp, type = "gtruth"))

  Z <- grep("Z", names(data), value = TRUE)
  W <- grep("W", names(data), value = TRUE)

  # increase non-overlap
  data[[Z[1]]][data$X == 1] <- data[[Z[1]]][data$X == 1] + 5

  expect_message(
    with_seed(
      203,
      fairness_cookbook(data, X = "X", Z = Z, Y = "Y", W = W,
                        x0 = 0, x1 = 1)
    ),
    regexp = "Estimates likely biased."
  )

})
