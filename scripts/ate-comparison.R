#!/usr/bin/env Rscript

#BSUB -W 4:00
#BSUB -n 16
#BSUB -R rusage[mem=4000]
#BSUB -J width[1-4]
#BSUB -o hypo/results/width_%J.out

library(ranger)
library(grf)

model_mean <- function(form, data, int.data, ...) {
  
  rf <- ranger(form, data = data, keep.inbag = T, importance = "impurity", ...)
  assertthat::assert_that(rf$treetype %in% c("Regression",
                                             "Probability estimation"))
  
  if (rf$treetype == "Probability estimation") {
    
    p2 <- predict(rf, int.data, predict.all = T)$predictions[, 2, ]
    
  } else {
    
    p2 <- predict(rf, int.data, predict.all = T)$predictions
    
  }
  
  oob.matrix <- Reduce(cbind, lapply(rf$inbag.counts, function(x=i) x == 0))
  
  rowSums(p2 * oob.matrix) / rowSums(oob.matrix)
  
}

model_propensity <- function(form, data, xlvl, ...) {
  
  assertthat::assert_that(length(xlvl) == 1L)
  
  rf <- ranger(form, data = data, keep.inbag = TRUE, importance = "impurity", 
               probability = TRUE, ...)
  assertthat::assert_that(rf$treetype == "Probability estimation")
  
  rf$predictions[, xlvl]
  
}

jid <- as.integer(Sys.getenv("LSB_JOBINDEX"))

pos_est <- function(n, seed) {
  
  set.seed(seed)
  
  test_scm <- function(n, f0, f1) {
    
    expit <- function(x) exp(x) / (1 + exp(x))
    
    Z <- replicate(5, runif(n, -1, 1))
    
    X <- rbinom(n, size = 1, prob = expit(rowMeans(Z)))
    
    Y <- f0(Z) + X * f1(Z) + rnorm(n, sd = 1/2)
    
    data.frame(cbind(X, Z, Y))
    
  }

  x1 <- 1
  x0 <- 0
  
  f0 <- function(x) rowSums(x)
  f1 <- function(x) rowSums((x^2)[, c(T, F, F)])
  dat <- test_scm(n, f0, f1)
  dat0 <- dat1 <- dat
  
  dat0$X <- x0
  dat1$X <- x1
  
  
  mu1 <- model_mean(Y ~ ., dat, dat1, probability = length(unique(dat$Y)) == 2L)
  mu0 <- model_mean(Y ~ ., dat, dat0, probability = length(unique(dat$Y)) == 2L)
  
  ATE <- mean(mu1 - mu0)
  
  prop1 <- model_propensity(X ~ . - Y, dat, xlvl = x1)
  
  idx <- dat$X == x1
  dr_mu1 <- 1 / n * sum( (dat$Y - mu1)[idx] / prop1[idx]  ) + mean(mu1)
  dr_mu0 <- 1 / n * sum((dat$Y - mu0)[!idx] / (1 - prop1[!idx])) + mean(mu0) 
  DR_ATE <- dr_mu1 - dr_mu0
  
  # causal forest approach
  crf <- causal_forest(
    X = dat[, !(names(dat) %in% c("X", "Y"))],
    Y = dat$Y,
    W = dat$X
  )
  
  c(ATE, DR_ATE, mean(crf$predictions))
  
}

grid <- c(100, 200, 300, 500)

est <- list()
for(i in seq_len(3)) {
  
  est[[i]] <- pos_est(n = grid[jid], seed = i)
  cat("Boot run", i, "finished; ")
}

est <- data.frame(Reduce(rbind, est))
names(est) <- c("RF", "Double Robust", "Causal Forest")

save(est, file = file.path("CFA", "res", paste0("est_", jid, ".RData")))

# ggplot(reshape2::melt(est), aes(x = value, fill = variable)) +
#   geom_density(alpha = 0.5) +
#   geom_vline(xintercept = 2/3, color = "red", linetype = "dashed") +
#   theme_bw() + xlab("Average Treatment Effect (ATE) estimate") +
#   ylab("Density")
