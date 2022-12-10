
test_that("fairness_cookbook input type consistency", {
  
  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })
  
  vars <- c("y", "x", "w", "z")
  
  expect_setequal(colnames(data), vars)
  
  fc.nms <- c("measures", "x0", "x1", "model", "X", "W", "Z", "Y", "cl",
              "eo")
  
  # apply cookbook to various data types of Y
  
  ran.fct <- with_seed(
    202,
    fairness_cookbook(data, X = "x", W = "w", Z = "z", Y = "y",
                      x0 = 0, x1 = 1)
  )
  
  data$y <- data$y == 1
  ran.logi <- with_seed(
    202,
    fairness_cookbook(data, X = "x", W = "w", Z = "z", Y = "y",
                      x0 = 0, x1 = 1)
  )
  
  data$y <- as.integer(data$y)
  ran.int <- with_seed(
    202,
    fairness_cookbook(data, X = "x", W = "w", Z = "z", Y = "y",
                      x0 = 0, x1 = 1)
  )
  
  # make sure they are identical
  expect_equal(ran.fct$measures, ran.logi$measures)
  expect_equal(ran.fct$measures, ran.int$measures)
  
  
  # test consistency with respect to x data type
  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })
  
  data$x <- factor(data$x, levels = c(0, 1))
  ran.xfct <- with_seed(
    202,
    fairness_cookbook(data, X = "x", W = "w", Z = "z", Y = "y",
                      x0 = 0, x1 = 1)
  )
  
  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })
  
  data$x <- ifelse(data$x == 0, "a", "b")
  ran.xchr <- with_seed(
    202,
    fairness_cookbook(data, X = "x", W = "w", Z = "z", Y = "y",
                      x0 = "a", x1 = "b")
  )
  
  expect_equal(ran.fct$measures, ran.xfct$measures)
  expect_equal(ran.fct$measures, ran.xchr$measures)
  
  # data.table input
  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })
  
  data <- data.table::as.data.table(data)
  
  ran.dtb <- with_seed(
    202,
    fairness_cookbook(data, X = "x", W = "w", Z = "z", Y = "y",
                      x0 = 0, x1 = 1)
  )
  
  expect_equal(ran.fct$measures, ran.dtb$measures)
  
})
