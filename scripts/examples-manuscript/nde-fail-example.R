
NDEfail_I <- function(alpha = 0.2) {
  
  expit <- \(x) exp(x) / (1 + exp(x))
  n <- 100000
  U <- rnorm(n)
  
  X <- rbinom(n, 1, expit(U))
  Z <- rbinom(n, 1, expit(U))
  
  W <- rbinom(n, 1, 0.3)
  Y <- rbinom(n, 1, alpha * (X + Z - 2 * X * Z) + 0.3 * W)
  
  nde <- (mean(Y[X == 1 & Z == 0]) - mean(Y[X == 0 & Z == 0])) * mean(Z == 0) +
         (mean(Y[X == 1 & Z == 1]) - mean(Y[X == 0 & Z == 1])) * mean(Z == 1)
  nde
}

NDEfail_I()

plot(
  seq(0, 0.35, 0.01),
  vapply(seq(0, 0.35, 0.01), NDEfail_I, numeric(1L))
)
