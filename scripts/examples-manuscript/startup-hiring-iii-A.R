library(ggplot2)

expit <- function(x) exp(x) / (1 + exp(x))

for (k in 1:20) {
  set.seed(k)
  
  n <- 100000
  u_xz <- rbinom(n, 1, 0.5)
  x <- u_xz # rbinom(n, 1, 0.5 + 0.5*u_xz)
  z <- -1*u_xz + runif(n)
  w <- x + z + runif(n)
  y <- rbinom(n, 1, expit(w))
  
  # TV
  cat("TV =", mean(y[x == 1]) - mean(y[x == 0]), "\n")
  
}

# density of W | x
df <- data.frame(x, z, w, y)

ggplot(df, aes(x = w, fill = factor(x))) +
  geom_density(alpha = 0.3) + theme_bw()
