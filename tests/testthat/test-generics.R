

with_seed(301, {
  data <- data_gen(100, add_z = TRUE)
})

vars <- c("x", "y", "w", "z")

expect_setequal(colnames(data), vars)

test_that("generics, ranger", {

  fc.ranger <- with_seed(
    302,
    fairness_cookbook(data, X = "x", Y = "y", Z = "z", W = "w", x0 = 0, x1 = 1)
  )

  announce_snapshot_file(name = "auto_ranger.png")
  announce_snapshot_file(name = "unsigned_ranger.png")
  announce_snapshot_file(name = "both_ranger.png")

  expect_snapshot(print(fc.ranger))
  expect_snapshot(summary(fc.ranger))
  expect_snapshot(summary(fc.ranger, decompose = "general"))

  aut.plt <- autoplot(fc.ranger)

  expect_s3_class(aut.plt, "ggplot")

  expect_snapshot_plot("auto_ranger", print(aut.plt))

  unsign.plt <- autoplot(fc.ranger, signed = FALSE)
  expect_snapshot_plot("unsigned_ranger", print(unsign.plt))

  both.plt <- autoplot(fc.ranger, decompose = "both")
  expect_snapshot_plot("both_ranger", print(both.plt))
})

test_that("generics, linear", {

  fc.lin <- with_seed(
    302,
    fairness_cookbook(data, X = "x", Y = "y", Z = "z", W = "w", x0 = 0, x1 = 1,
                      model = "linear")
  )

  announce_snapshot_file(name = "auto_lin.png")

  expect_snapshot(print(fc.lin))
  expect_snapshot(summary(fc.lin))

  aut.plt <- autoplot(fc.lin, decompose = "general")

  expect_s3_class(aut.plt, "ggplot")

  expect_snapshot_plot("auto_lin", print(aut.plt))
})
