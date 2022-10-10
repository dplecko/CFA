
n <- 1000000

u1 <- rbinom(n, 1, 0.5)
u2 <- rbinom(n, 1, 0.5)
ux <- rbinom(n, 1, 0.5)

z1 <- u1
z2 <- z1 * u2
x <- z1 * z2 * ux

# conditional
mean(z1[x == 0]) - 3/7

# fixing u2
1 / 2 * (mean(z1[x == 0 & u2 == 0]) + mean(z1[x == 0 & u2 == 1])) - 5/12

# x distribution
mean(x == 0)


(1 / 2 * mean(z1[u2 == 0] == 1 & x[u2 == 0] == 0) +
    1 / 2 * mean(z1[u2 == 1] == 1 & x[u2 == 1] == 0)) /
  (1 / 2 * mean(x[u2 == 0] == 0) + 1 / 2 * mean(x[u2 == 1] == 0)) - 3/7


sum <- 0
for (z2p in unique(z2)) {
  add <- mean(z2[z1 == 1] == z2p) * mean(z1[x == 0] == 1 & z2[x == 0] == z2p)
  cat("Adding", add, "\n")
  sum <- sum + add
}
sum - 5/12


### canonical types
check_scm <- function(n = 10000000, prob) {

  canon <- function(z, u) {
    z2 <- rep(0, length(z))

    z2[u == 1] <- z[u == 1]
    z2[u == 2] <- 1 - z[u == 2]
    z2[u == 3] <- 1
    z2[u == 4] <- 0

    z2
  }

  u1 <- rbinom(n, 1, 0.5)
  u2 <- sample(1:4, size = n, prob = prob, replace = TRUE)
  ux <- rbinom(n, 1, 0.5)
  ux1 <- rbinom(n, 1, 0.5)
  ux2 <- rbinom(n, 1, 0.5)

  z1 <- u1
  z2 <- canon(z1, u2)
  x <- (z1 & ux1) | (z2 & ux2) | ux
  y <- x + z1 + z2

  #cat("P(Z2 = 1 | Z1 = 1) =", round(100 * mean(z2[z1 == 1]), 2), "\n")
  #cat("P(Z2 = 1 | Z1 = 0) =", round(100 * mean(z2[z1 == 0]), 2), "\n")

  sum <- 0

  for (u2p in 1:4) if (sum(u2 == u2p) > 0) {
    sum <- sum + prob[u2p] * mean(z1[x == 0 & u2 == u2p] == 1)
  }

  # cat(mean((u2 %in% c(1,3))[x == 0]), "\n\n")
  #
  # cat((mean(z2[z1 == 1 & x == 0])), "\n\n")

  show <- FALSE
  if (show) {
    cat("P(u2 in {1,3}, X = 0, Z1 = 0)",
        mean((u2 %in% c(1,3)) & x == 0 & z1 == 0), "\n")

    cat("Alternative = ",
        mean(z1 == 0) * (
          mean(u2 == 1) * mean(x[z1 == 0 & z2 == 0] == 0) +
            mean(u2 == 3) * mean(x[z1 == 0 & z2 == 1] == 0)
        ), "\n")

    cat("P(Z1 = 0)", mean(z1 == 0), "\n")

    cat("P(U2 = 1) * P(x0 | z1 = 0, z2 = 0) is",
        mean(u2 == 1), "*", mean(x[z1 == 0 & z2 == 0] == 0), "\n")

    cat("P(U2 = 3) * P(x0 | z1 = 0, z2 = 1) is",
        mean(u2 == 3), "*", mean(x[z1 == 0 & z2 == 1] == 0), "\n")

    cat(mean((u2 %in% c(1,3))[x == 0]), "\n\n")
  }



  cond <- vapply(1:4, function(u2p) mean(y[u2 == u2p & x == 0]), numeric(1L))
  idx <- !is.nan(cond)
  sum(
     cond[idx] * prob[idx]
  )
}


check_scm(prob = c(0, 1/4, 1/2, 1/4))

check_scm(prob = c(1/4, 1/2, 1/4, 0))

# check_scm(prob = c(1/8, 3/8, 3/8, 1/8))

