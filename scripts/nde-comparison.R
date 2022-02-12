library(causalweight)
library(readr)
library(rockchalk)
library(ranger)
library(ggplot2)
library(latex2exp)
library(stringr)
library(grf)
library(assertthat)

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

pos_est_de <- function(n, f0, f1, seed = 2022, model = "ranger") {
  
  set.seed(seed)
  
  x1 <- 1
  x0 <- 0
  
  dat <- test_scm(n, f0, f1, type = "dat")
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
  
  res_my <- CausalExplanation_TV(dat, X = "X", Z = c("Z1", "Z2", "Z3"),
                                 W = c("W1", "W2", "W3"), Y = "Y", x0 = x0,
                                 x1 = x1, model = model)
  
  c(mean(mu1) - mean(mu0), res$results["effect", "dir.treat"],
    res_my$NDE[1])
    
}

# pick the SCM functions
f0 <- \(x) rowMeans(abs(x))
f1 <- \(x) rowSums((x^2 * max(1, log(abs(x))))[, c(T, F)])

# get ground truth
g_truth <- test_scm(n = 1000000, f0, f1, type = "nde")

# choose sample size and model
nsamp <- 2000L
nboot <- 5L
model <- "ranger"

# estimate over 50 bootstrap repetitions
est <- list()
for(i in seq_len(nboot)) {
  
  est[[i]] <- pos_est_de(n = nsamp, f0, f1, seed = i, model = model)
  cat("Boot run", i, "finished; ")
}

est <- data.frame(Reduce(rbind, est))
names(est) <- c("RF", "Double Robust Mediated", "DML own")

ggplot(reshape2::melt(est), aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = g_truth, color = "red", linetype = "dashed") +
  theme_bw() + xlab("Natural Direct Effect (NDE) estimate") +
  ylab("Density")
