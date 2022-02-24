

berkeley_gen <- function(lambda, alpha, beta) {
  
  if (lambda > 0.2) return(NA_real_)
  if (0.1 + alpha + 6*beta > 1) return(NA_real_)
  n <- 100000
  x <- rbinom(n, 1, 0.5)
  
  ud <- runif(n, 0, 5)
  d <- 1 + floor(ud + 5*lambda*x)
  
  y <- rbinom(n, 1, prob = (alpha*x + beta*d))
  #browser()
  mean(y[x == 1]) - mean(y[x == 0])
  
}

dir_seq <- vapply(
  seq(0, 0.6, 0.01),
  \(x) berkeley_gen(lambda = 1/6, alpha = x, beta = 0),
  numeric(1L)
)

par(mfrow = c(1, 2))

plot(
  seq(0, 0.6, 0.01), dir_seq, pch = 19, col = "blue"
)
abline(h = 0.14)

indir_seq <- vapply(
  seq(0, 0.2, 0.005),
  \(x) berkeley_gen(lambda = x, alpha = 0, beta = 1/7),
  numeric(1L)
)

plot(
  seq(0, 0.2, 0.005), indir_seq, pch = 19, col = "red"
)
abline(h = 0.14)

berkeley_gen(0.2, 0, 1/7)
