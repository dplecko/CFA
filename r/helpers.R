
RejectOption <- function(prob, cl) {
  
  rate_cl <- tapply(prob, cl, function(x) mean(x > 0.5))
  fav_cl <- names(rate_cl)[which.max(rate_cl)]
  
  theta <- 0.5 + seq(0, 0.5, 0.005)
  tv <- vapply(
    theta,
    function(th) {
      mean(prob[cl == fav_cl] > th) - mean(prob[cl != fav_cl] > 1 - th) 
    }, numeric(1L)
  )
  
  theta_min <- theta[which.min(abs(tv))]
  
  prob[cl == fav_cl] <- prob[cl == fav_cl] > theta_min
  prob[cl != fav_cl] <- prob[cl != fav_cl] > 1 - theta_min
  
  prob
  
}

test_scm <- function(n, f0, f1, type = "dat") {
  
  expit <- function(x) exp(x) / (1 + exp(x))
  
  Z <- replicate(3, runif(n, -1, 1))
  
  X <- rbinom(n, size = 1, prob = expit(rowMeans(abs(Z))))
  
  W1 <- rnorm(n) + X * rowMeans(Z[, c(1, 2)])
  W2 <- rnorm(n) + W1^2 / 2 - 1 + X * rowMeans((Z^2)[, c(2, 3)])
  W3 <- rnorm(n) + W1 * W2 / 6 + rnorm(n) + X * Z[, 1] / 4
  
  
  
  if (type == "dat") {
    Y <- f0(cbind(Z, W1, W2, W3)) + X * f1(cbind(Z, W1, W2, W3)) + 
      rnorm(n, sd = 1/2)
    dat <- data.frame(cbind(X, Z, W1, W2, W3, Y))
    names(dat) <- c("X", paste0("Z", seq_len(ncol(Z))),
                    "W1", "W2", "W3", "Y")
    return(dat)
  } else if (type == "nde") {
    return(mean(f1(cbind(Z, W1, W2, W3))))
  } else if (type == "ctfde") {
    return(mean(f1(cbind(Z, W1, W2, W3)[X == 0, ])))
  }
  
}

