
test_that("nboot and tune_params work", {

  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })
  data$y <- as.numeric(data$y) - 1L

  for (method in c("medDML", "causal_forest")) {

    for (nboot1 in c(1, 2)) {

      for (nboot2 in c(1, 2)) {

        for (tune_params in c(FALSE, TRUE)) {

          # apply cookbook to various data types of Y
          ran <- with_seed(
            202,
            fairness_cookbook(data, X = "x", W = "w", Z = "z", Y = "y",
                              x0 = 0, x1 = 1, method = method,
                              tune_params = tune_params, nboot1 = nboot1,
                              nboot2 = nboot2)
          )

          expect_equal(nrow(ran$measures), 10 * nboot1 * nboot2)
        }
      }
    }

  }
})
