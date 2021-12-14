library(causalweight)
library(readr)
library(rockchalk)
library(ranger)
library(ggplot2)
library(latex2exp)
library(stringr)
library(grf)

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

pos_est_de <- function(n, seed) {
  
  set.seed(seed)
  
  test_scm <- function(n, f0, f1) {
    
    expit <- function(x) exp(x) / (1 + exp(x))
    
    Z <- replicate(3, runif(n, -1, 1))
    
    X <- rbinom(n, size = 1, prob = expit(rowMeans(Z)))
    
    W1 <- rnorm(n) + X * rowMeans(Z[, c(1, 2)])
    W2 <- rnorm(n) + W1^2 / 2 - 1 + X * rowMeans((Z^2)[, c(2, 3)])
    W3 <- rnorm(n) + W1 * W2 / 6 + rnorm(n) + X * Z[, 1] / 4
    
    Y <- f0(cbind(Z, W1, W2, W3)) + X * f1(cbind(Z, W1, W2, W3)) + 
         rnorm(n, sd = 1/2)
    
    dat <- data.frame(cbind(X, Z, W1, W2, W3, Y))
    names(dat) <- c("X", paste0("Z", seq_len(ncol(Z))),
                    "W1", "W2", "W3", "Y")
    
    dat
    
  }

  x1 <- 1
  x0 <- 0
  
  f0 <- function(x) rowSums(x)
  f1 <- function(x) rowSums((x^2)[, c(T, F)])
  dat <- test_scm(n, f0, f1)
  dat0 <- dat1 <- dat
  
  dat0$X <- x0
  dat1$X <- x1
  
  mu1 <- model_mean(Y ~ ., dat, dat1, 
                    probability = length(unique(dat$Y)) == 2L)
  mu0 <- model_mean(Y ~ ., dat, dat0, 
                    probability = length(unique(dat$Y)) == 2L)
  
  res <- medDML(
    y = dat$Y,
    d = dat$X,
    m = dat[, c("W1", "W2", "W3")],
    x = dat[, c("Z1", "Z2", "Z3")]
  )
  
  c(mean(mu1) - mean(mu0), res$results["effect", "dir.treat"])
    
  
}

est <- list()
for(i in seq_len(100)) {
  
  est[[i]] <- pos_est_de(n = 2000, seed = i)
  cat("Boot run", i, "finished; ")
}

est <- data.frame(Reduce(rbind, est))
names(est) <- c("RF", "Double Robust Mediated")

ggplot(reshape2::melt(est), aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 2.4, color = "red", linetype = "dashed") +
  theme_bw() + xlab("Natural Direct Effect (NDE) estimate") +
  ylab("Density")
