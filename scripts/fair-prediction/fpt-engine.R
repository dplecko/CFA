library(reshape2)
library(ggplot2)
library(latex2exp)

expit <- function(x) exp(x) / (1+exp(x))

up_diag <- function(n) {
  A <- matrix(runif(n*n, -1, 1), ncol = n)
  for(i in seq_len(n)) {
    for (j in seq_len(i)) {
      A[i, j] <- 0
    }
  }
  A
}

du01 <- -4 * integrate(function(x) 1 / (1+exp(x)) * 1/sqrt(2*pi)*exp(-x^2/2) * x, 
                       -Inf, Inf)$value
Exu <- -integrate(function(x) (1 / (1+exp(x))) * 1/sqrt(2*pi)*exp(-x^2/2) * x, 
                 -Inf, Inf)$value

pow <- function(x, n) Reduce(`%*%`, replicate(n, x, simplify = FALSE))

SCM_gen <- function(n, n_z, n_w, A_uz, A_zz, A_zw, A_ww, A_xw,
                    type = c("cov", "cvec", "TV"), A_vy = NULL) {
  U <- rnorm(n)
  X <- rbinom(n, 1, expit(U)) - 1/2
  
  eps_Z <- array(rnorm(n * n_z), dim = c(n, n_z))
  Z <- (eps_Z + U %*% t(A_uz)) %*% (solve((diag(n_z) - A_zz)))
  
  eps_W <- array(rnorm(n * n_w), dim = c(n, n_w))
  W <- (eps_W + X %*% t(A_xw) + Z %*% A_zw) %*% (solve((diag(n_w) - A_ww)))
  
  dat <- cbind(X, Z, W)
  if (type == "cov") {
    return(cov(dat))
  } else if (type == "cvec") {
    return(colMeans(dat[dat[, "X"] == 0.5, ]) - colMeans(dat[dat[, "X"] == -0.5, ]))
  } else {
    Y <- as.vector(dat %*% A_vy)
    return(mean(Y[dat[, "X"] == 0.5]) - mean(Y[dat[, "X"] == -0.5]))
  }
  
}

mat_path <- function(mat, n) {
  if (n == 0) {
    return(diag(dim(mat)[1]))
  } else {
    return(mat %*% (diag(dim(mat)[1]) + mat_path(mat, n-1))) 
  }
}

fpt_eng <- function(n_z = 1, n_w = n_z, seed = 7) {
  
  set.seed(seed)
  V_names <- c("X", paste0("Z", seq_len(n_z)), paste0("W", seq_len(n_w)))
  
  A_xwxw <- up_diag(n_w+1)
  A_xw <- A_xwxw[1, -1]
  A_ww <- A_xwxw[-1, -1]
  
  A_zz <- up_diag(n_z)
  A_zw <- array(runif(n_z*n_w), dim = c(n_z, n_w))
  
  A_uz <- runif(n_z, -1, 1)
  
  A_vy <- runif(n_z + n_w + 1, -1, 1)
  
  dZ <- t(solve(diag(n_z) - A_zz)) %*% A_uz * du01
  
  cvec <- c(
    1,
    dZ,
    mat_path(A_xwxw, n_w + 1)[1, -1] + 
      as.vector(solve(diag(n_w) - t(A_ww)) %*% t(A_zw) %*% dZ)
  )
  
  A <- rbind(
    c(0, 0, A_uz, rep(0, n_w)),
    c(0, 0, rep(0, n_z), A_xw),
    cbind(array(0, dim = c(n_z, 2)), A_zz, A_zw),
    cbind(array(0, dim = c(n_w, n_z + 2)), A_ww)
  )
  rownames(A) <- colnames(A) <- c("U", V_names)
  
  piA <- mat_path(A, n_z + n_w + 2) + diag(n_z + n_w + 2)
  weigh_mat <- diag(n_z + n_w + 2)
  weigh_mat[, 2] <- weigh_mat[, 2] / 4
  
  rownames(piA) <- colnames(piA) <- c("U",  V_names)
  
  Sigma <- t(piA) %*% weigh_mat %*% piA + 
    Exu * ((piA[2, ]) %*% t(piA[1, ]) + (piA[1, ]) %*% t(piA[2, ]))
  Sigma <- Sigma[-1, -1] # remove the column corresponding to U
  
  ### verify the covariance is correct
  
  #delta_Sigma <- Sigma - Sigma_hat
  #rownames(delta_Sigma) <- colnames(delta_Sigma) <- V_names

  # ggplot(melt(delta_Sigma), aes(x = Var1, y = Var2, fill = value)) +
  #   geom_tile() + scale_fill_viridis_c()
  
  alpha_hat <- A_vy - as.vector(cvec %*% A_vy) * as.vector(solve(Sigma) %*% cvec) / 
    as.vector(t(cvec) %*% solve(Sigma) %*% cvec)
  
  fde <- sign(alpha_hat[1]) * min(c(abs(alpha_hat[1]), 1))
  
  c(alpha_hat[1], A_vy[1])
}

res <- NULL
for (seed in seq_len(10000)) {
  res <- rbind(res, fpt_eng(n_z = 5, seed = seed))
  print(seed)
}

res <- data.frame(res)
names(res) <- c("FairDE", "DE")

ggplot(res, aes(x = abs(FairDE))) +
  stat_ecdf() + theme_bw() +
  xlab(TeX("$\\epsilon$")) +
  ylab(TeX("$P(M \\in H^{DE}(\\epsilon))$")) +
  xlim(c(0, 2.5)) +
  ggtitle(TeX("Probability of $\\epsilon$-TV-compliant SCM")) +
  theme(
    axis.title = element_text(size = 14),
    title = element_text(size = 18)
  )

root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
ggsave(file.path(root, "paper", "figures", "fpt-empirical.png"),
       width = 8, height = 5)

# ggplot(res, aes(x=DE, y=FairDE) ) +
#   stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
#   scale_fill_distiller(palette= "Spectral", direction=1) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(-2, 2)) +
#   theme(
#     legend.position="bottom"
#   ) + geom_abline(slope = 1, intercept = 0)
