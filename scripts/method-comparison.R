library(readr)
library(rockchalk)
library(ranger)
library(ggplot2)
library(latex2exp)
library(stringr)
library(grf)
library(zeallot)

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

pos_est <- function(n, seed) {
  
  set.seed(seed)
  
  test_scm <- function(n, f0, f1) {
    
    expit <- function(x) exp(x) / (1 + exp(x))
    
    Z <- replicate(3, runif(n, -1, 1))
    
    X <- rbinom(n, size = 1, prob = expit(1 / 3 * rowSums(Z)))
    
    Y <- f0(Z) + X * f1(Z) + rnorm(n, sd = 1/2)
    
    data.frame(cbind(X, Z, Y))
    
  }

  x1 <- 1
  x0 <- 0
  
  f0 <- function(x) rowSums(x)
  f1 <- function(x) rowSums((x^2)[, c(T, F)])
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


est <- list()
for(i in seq_len(100)) {
  
  est[[i]] <- pos_est(n = 2000, seed = i)
  cat("Boot run", i, "finished; ")
}

est <- data.frame(Reduce(rbind, est))
names(est) <- c("RF", "Double Robust", "Causal Forest")

ggplot(reshape2::melt(est), aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 2/3, color = "red", linetype = "dashed") +
  theme_bw() + xlab("Average Treatment Effect (ATE) estimate") +
  ylab("Density")
