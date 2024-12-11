
test_that("fairness_cookbook osd variants", {
  
  with_seed(201, {
    data <- data_gen(100, add_z = TRUE)
  })
  vars <- c("y", "x", "w", "z")
  X <- "x" 
  W <- "w"
  Z <- "z" 
  Y <- "y"
  
  expect_setequal(colnames(data), vars)
  
  fc.nms <- c("measures", "x0", "x1", "model", "X", "W", "Z", "Y", "cl",
              "method", "params", "pw")
  
  for (type in c("", "Z", "W", "ZW")) {
    
    for (log_risk in c(FALSE, TRUE)) {
      
      if (type == "ZW") {
        
        Z <- W <- NULL
      } else if (type == "Z") {
        
        Z <- NULL
        W <- "w"
      } else if (type == "W") {
        
        Z <- "z"
        W <- NULL
      }
      
      # apply cookbook to various data types of Y
      ran <- with_seed(
        202,
        fairness_cookbook(data, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, 
                          method = "debiasing", log_risk = log_risk)
      )
      
      # both print() and str() throw
      expect_type(ran, "list")
      expect_named(ran, fc.nms, ignore.order = TRUE)
      
      expect_s3_class(ran, "faircause")
      expect_type(ran[["measures"]], "list")
      expect_identical(ran[["X"]], "x")
      expect_identical(ran[["method"]], "debiasing")
      
      
      ran.meas <- ran[["measures"]]
      
      meas.nms <- c("tv", "ctfde", "ctfse", "ett", "ctfie")
      col.nms <- c("value", "measure", "sd", "scale")
      expect_named(ran.meas, col.nms, ignore.order = TRUE)
      expect_setequal(unique(ran.meas$measure), meas.nms)
    }
  }
})