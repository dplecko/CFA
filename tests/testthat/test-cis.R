
# source("tests/testthat/helpers-correctness.R")
# source("tests/testthat/helpers.R")

test_that("CIs have faithful coverage", {
  
  #' * change 'SLOW' to true to test correctness *
  Sys.setenv(SLOW = "false")
  skip_if_not(identical(Sys.getenv("SLOW"), "true"))
  
  # experiment parameters
  nsamp <- 1000
  nrep <- 100
  methods <- c("medDML", "causal_forest")
  exp_range <- c(1, 2, 3)
  alpha_range <- seq(0.05, 0.4, 0.01)
  alpha_point <- 0.1
  
  # (no)-mediators example
  for (i in exp_range) {
    
    ci_cov <- NULL
    dat_fun <- ifelse(i == 1, ex_nomed, ifelse(i == 2, ex_med, ex_med_spur))
    
    gtruth <- with_seed(203, dat_fun(10^6, type = "gtruth"))
    df_truth <- data.frame(measure = names(gtruth), truth = unlist(gtruth))
    
    Z <- grep("Z", names(data), value = TRUE)
    W <- grep("W", names(data), value = TRUE)
    
    for (j in seq_len(nrep)) {
      
      cat("\r", j, "rep starting...")
      data <- with_seed(j, dat_fun(nsamp))
      
      for (method in methods) {
        
        mod <- with_seed(
          203,
          fairness_cookbook(data, X = "X", Z = Z, Y = "Y", W = W,
                            x0 = 0, x1 = 1, method = method,
                            nboot1 = 5, nboot2 = 100, tune_params = TRUE)
        )
        
        df_meas <- summary(mod)$measures
        df_meas <- merge(df_meas, df_truth)
        df_meas$rep <- j
        df_meas$method <- method
        
        ci_cov <- rbind(ci_cov, df_meas)
      }
    }
    
    # fixed alpha_point
    ci_cov <- data.table::as.data.table(ci_cov)
    ci_len <- qnorm(1 - alpha_point / 2)
    ci_cov[, cov := (truth <= value + sd * ci_len) &  (truth >= value - sd * ci_len)] 
    
    ci_covp <- ggplot(
      ci_cov[, list(Coverage = mean(cov)), by = c("measure", "method")],
      aes(x = measure, y = Coverage, color = method)
    ) + geom_point() + theme_bw() +
      geom_hline(yintercept = 1 - alpha_point, color = "red", linetype = "dashed") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1))
    
    expect_snapshot_plot(paste0("ci_cov", i), print(ci_covp))
    
    # over a range of alpha
    alpha_reps <- lapply(
      alpha_range, 
      function(a) {
        ci_len <- qnorm(1 - a / 2)
        ci_rep <- copy(ci_cov)
        ci_rep[, cov := (truth <= value + sd * ci_len) & 
                        (truth >= value - sd * ci_len)]
        ci_rep[, alpha := a]
        ci_rep
      } 
    )
    alpha_reps <- do.call(rbind, alpha_reps)
    
    alpha_covp <- ggplot(
      alpha_reps[, list(Coverage = mean(cov)), by = c("measure", "method", "alpha")],
      aes(x = 1 - alpha, y = Coverage, color = measure, linetype = method)
    ) + geom_line() + theme_bw() +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dotdash",
                  linewidth = 1)
    
    expect_snapshot_plot(paste0("alpha_cov", i), print(alpha_covp))
      
  }
  
})