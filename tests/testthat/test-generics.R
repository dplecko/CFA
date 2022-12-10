

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
  # announce_snapshot_file(name = "plot_ranger.png")
  # announce_snapshot_file(name = "graph_ranger.png")
  # announce_snapshot_file(name = "ftdef_ranger.csv")
  # announce_snapshot_file(name = "fttrn_rf.csv")
  # announce_snapshot_file(name = "predi_rf.csv")

  expect_snapshot(print(fc.ranger))
  expect_snapshot(summary(fc.ranger))

  aut.plt <- autoplot(fc.ranger)

  expect_s3_class(aut.plt, "ggplot")

  expect_snapshot_plot("auto_ranger", print(aut.plt))
  # expect_snapshot_plot("graph_rf", with_seed(302, visualizeGraph(ad.rf)))

  # expect_snapshot_csv("fttrn_rf", fairTwins(ad.rf, train.id = NULL,
  #                                           test.id = 1L))
  # expect_snapshot_csv("ftdef_rf", fairTwins(ad.rf), arch = "x86_64")
  # expect_snapshot_csv("predi_rf", with_seed(302, predict(ad.rf, pred)),
  #                     arch = "x86_64")
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
  # expect_snapshot_plot("graph_rf", with_seed(302, visualizeGraph(ad.rf)))

  # expect_snapshot_csv("fttrn_rf", fairTwins(ad.rf, train.id = NULL,
  #                                           test.id = 1L))
  # expect_snapshot_csv("ftdef_rf", fairTwins(ad.rf), arch = "x86_64")
  # expect_snapshot_csv("predi_rf", with_seed(302, predict(ad.rf, pred)),
  #                     arch = "x86_64")
})
