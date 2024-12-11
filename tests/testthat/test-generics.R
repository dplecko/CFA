
with_seed(301, {
  data <- data_gen(100, add_z = TRUE)
})

vars <- c("x", "y", "w", "z")

expect_setequal(colnames(data), vars)

test_that("test generics for combinations of method, model", {

  methods <- c("medDML", "causal_forest", "debiasing")
  models <- c("ranger", "linear")

  for (method in methods) {
    for (model in models) {

      if (method != "medDML" & model == "linear") next
      cat(method, model, "\n")
       # Use local scope to avoid variable overwriting issues in loops
      local({
        method_inner <- method
        model_inner <- model

        fc <- with_seed(
          302,
          fairness_cookbook(data, X = "x", Y = "y", Z = "z", W = "w",
                            x0 = 0, x1 = 1, method = method_inner, model = model_inner)
        )

        auto_plot_name <- paste0("auto_", model_inner, "_", method_inner)
        unsigned_plot_name <- paste0("unsigned_", model_inner, "_", method_inner)
        both_plot_name <- paste0("both_", model_inner, "_", method_inner)

        # Announce snapshot files
        announce_snapshot_file(name = paste0(auto_plot_name, ".png"))

        # Expectations
        expect_snapshot(print(fc))
        expect_snapshot(summary(fc))
        expect_snapshot(summary(fc, decompose = "general"))

        # Autoplot
        aut.plt <- autoplot(fc)
        expect_s3_class(aut.plt, "ggplot")
        expect_snapshot_plot(auto_plot_name, print(aut.plt))

        # For ranger model, include unsigned and both plots
        if (model_inner == "ranger") {
          announce_snapshot_file(name = paste0(unsigned_plot_name, ".png"))
          announce_snapshot_file(name = paste0(both_plot_name, ".png"))

          unsign.plt <- autoplot(fc, signed = FALSE)
          expect_snapshot_plot(unsigned_plot_name, print(unsign.plt))

          if (method == "medDML") {

            both.plt <- autoplot(fc, decompose = "both")
            expect_snapshot_plot(both_plot_name, print(both.plt))
          }
        }
      })
    }
  }
})

