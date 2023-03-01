
# source("tests/testthat/helpers-correctness.R")
# source("tests/testthat/helpers.R")

test_that("fairness_cookbook correctness", {

  #' * change 'SLOW' to true to test correctness *
  Sys.setenv(SLOW = "false")
  skip_if_not(identical(Sys.getenv("SLOW"), "true"))

  # (no)-mediators example
  for (i in c(1, 2, 3)) {

    dat_fun <- ifelse(i == 1, ex_nomed, ifelse(i == 2, ex_med, ex_med_spur))
    cat("\n---- next example ----\n")
    nsamp <- 5000
    data <- with_seed(
      2023, dat_fun(nsamp)
    )

    gtruth <- with_seed(203, dat_fun(10^6, type = "gtruth"))

    Z <- grep("Z", names(data), value = TRUE)
    W <- grep("W", names(data), value = TRUE)

    for (method in c("medDML", "causal_forest")) {

      mod <- with_seed(
        203,
        fairness_cookbook(data, X = "X", Z = Z, Y = "Y", W = W,
                          x0 = 0, x1 = 1, method = method,
                          nboot1 = 5, nboot2 = 100, tune_params = TRUE)
      )

      df_meas <- summary(mod)$measures

      for (meas in df_meas$measure) {

        est_meas <- df_meas[df_meas$measure == meas, ]$value
        sd_meas <- df_meas[df_meas$measure == meas, ]$sd
        p.val <- 2 * (1 - pnorm(abs(gtruth[[meas]] - est_meas),
                                sd = max(sd_meas, 10^(-16))))

        if (method == "causal_forest" & grepl("expse", meas)) {

          expect_true(is.na(p.val))
        } else {

          if (p.val < 0.0045) cat("\n", i, method, meas, p.val, "\n")
          expect_gt(p.val, 0.0045)
        }

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

  compas.decomp <- mod.sum$measures

  expect_snapshot_csv("compas_measures", compas.decomp)
})
