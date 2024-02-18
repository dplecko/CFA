
library(mvtnorm)
library(plotly)
library(reshape2)

density_one <- function(a, b, delta = 0.5) {

  dmvnorm(cbind(a,b), c(-2, -2), sigma = delta^2 * diag(2))

  # 1/2 * dmvnorm(cbind(a,b), c(0, 1), sigma = delta^2 * diag(2)) +
  #   1/2 * dmvnorm(cbind(a,b), c(0, -1), sigma = delta^2 * diag(2))

}

density_two <- function(a, b, delta = 0.5, eps = 0.2) {

  dmvnorm(cbind(a,b), c(-2, -2), sigma = delta^2 * diag(2))

  # 1/2 * dmvnorm(cbind(a,b), c(eps, sqrt(1-eps^2)), sigma = delta^2 * diag(2)) +
  #   1/2 * dmvnorm(cbind(a,b), c(-eps, -sqrt(1-eps^2)), sigma = delta^2 * diag(2))

}

delta <- 1
eps <- 0.4

x <- y <- seq(-5, 5, length.out = 200)
df <- expand.grid(x = x, y = y)

z1 <- matrix(density_one(df$x, df$y, delta = delta), nrow = 200)
z2 <- matrix(density_two(df$x, df$y, delta = delta, eps = eps), nrow = 200)

fig <- plot_ly(x = x, y = y) %>% layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, gridcolor = "white"),
                                        yaxis = list(showgrid = FALSE, showticklabels = FALSE))

fig <- fig %>% add_trace(z = ~z1, type = "surface",
                         colorscale = list(c(0, 1), c("orange", "red")),
                         opacity = 0.5)

fig <- fig %>% add_trace(z = ~z2, type = "surface",
                         colorscale = list(c(0, 1), c("darkblue", "blue")),
                         opacity = 0.5)

fig <- fig
# %>% add_surface()
