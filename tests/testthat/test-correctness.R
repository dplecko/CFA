
test_that("fairness_cookbook correctness", {

  #' * change 'SLOW' to true to test correctness *
  withr::local_envvar(SLOW = "false")
  skip_if_not(identical(Sys.getenv("SLOW"), "true"))

  # (no)-mediators example
  for (dat_fun in c(ex_nomed, ex_med)) {

    nsamp <- 10000
    data <- with_seed(
      203, dat_fun(nsamp)
    )

    gtruth <- with_seed(203, dat_fun(nsamp, type = "gtruth"))

    Z <- grep("Z", names(data), value = TRUE)
    W <- grep("W", names(data), value = TRUE)
    mod <- with_seed(
      203,
      fairness_cookbook(data, X = "X", Z = Z, Y = "Y", W = W,
                        x0 = 0, x1 = 1)
    )

    for (meas in names(mod$measures)) {

      if (meas == "ETT" & identical(dat_fun, ex_med)) {

        expect_lte(abs(gtruth[[meas]] - mod$measures[[meas]][1]), 0.2)

      } else {
        p.val <- 2 * (1 - pnorm(abs(gtruth[[meas]] - mod$measures[[meas]][1]),
                                sd = mod$measures[[meas]][2]))

        expect_gte(p.val, 0.01)
      }

    }
  }

  # real data correctness (COMPAS)
  data <- get(data("compas", package = "faircause"))

  Z <- c("age", "sex")
  W <- c("juv_fel_count", "juv_misd_count", "juv_other_count",
         "priors_count","c_charge_degree")
  X <- "race"
  Y <- "two_year_recid"

  mod <- with_seed(
    203,
    fairness_cookbook(data, X = X, Z = Z, Y = Y, W = W,
                      x0 = "White", x1 = "Non-White")
  )

  expect_output(print(mod), regexp = "faircause object:")
  mod.sum <- summary(mod)
  expect_s3_class(mod.sum, "summary.faircause")
  expect_output(print(mod.sum), regexp = "Call:")

  compas.decomp <- mod$measures

  expect_snapshot_json(compas.decomp, tolerance = 0.03)
})
