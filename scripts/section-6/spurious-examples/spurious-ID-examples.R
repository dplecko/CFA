
n <- 10^6

u1 <- rbinom(n, size = 1, prob = 0.5)
u2 <- rbinom(n, size = 1, prob = 0.5)
u1x <- rbinom(n, size = 1, prob = 0.5)
u2x <- rbinom(n, size = 1, prob = 0.5)
ux <- rbinom(n, size = 1, prob = 0.5)

z1 <- u1 * u1x
z2 <- u2 | u2x
x <- ux * (u1x | u2x)
y <- x + z1 + z2
y_x1 <- 1 + z1 + z2


mean(z2[x == 1]) - 5 / 6
mean(z1[x == 1]) - 2 / 6
mean(y[x == 1]) - 13/6

mean(y_x1)

# get y | x1^{U1x}
mean(y[x == 1 & u1x == 1]) * mean(u1x == 1) +
  mean(y[x == 1 & u1x == 0]) * mean(u1x == 0) - 17 / 8


res <- 0
for (z1p in c(0, 1)) {

  for (z2p in c(0, 1)) {

    res <- res + mean(y[z1 == z1p & z2 == z2p & x == 1]) *
                 mean(z2[x == 1] == z2p) *
                 mean(z1[x == 1] == z1p)
  }
}
res - 13 / 6

mean(z2[x == 1] == z2p) * mean(z1[x == 1] == z1p) -
  mean(z2[x == 1] == z2p & z1[x == 1] == z1p)

# other example

ufs_1 <- c(1 / 8, 1 / 8, 1 / 8, 1 / 8, 1 / 8, 1 / 8, 1 / 8, 1 / 8)
# ufs_2 <- c(1 / 8, 1 / 8, 1 / 8, 1 / 8, 0, 1 / 4, 1 / 4, 0)
ufs_3 <- c(0, 1 / 4, 1/4, 0, 0, 1 / 4, 1 / 4, 0)

eval_q <- function(ufs) {

  pz2 <- function(x, ctp, z2p) {

    if (is.element(ctp, c(2, 6))) return(x == z2p)
    if (is.element(ctp, c(3, 7))) return(x != z2p)
    if (z2p == 1) return(0.6)
    return(0.4)
  }

  res <- 0
  for (ctp in seq_along(ufs)) {

    ey <- 0
    for (z2p in c(0, 1)) {

      ey <- ey + ((ctp > 4) | z2p) * pz2(x = 1, ctp = ctp, z2p = z2p)
    }
    res <- res + ey * ufs[ctp]
  }
  res
}

eval_q(ufs_1)
eval_q(ufs_3)

pv <- function(ufs) {

  df <- expand.grid(z1 = c(0, 1), z2 = c(0, 1), x = c(0, 1), p = 0)
  df <- as.data.table(df)
  for (ctp in seq_along(ufs)) {

    for (z2p in c(0, 1)) {

      pz2 <- ifelse(z2p == 1, 0.6, 0.4)

      prob <- ufs[ctp] * pz2

      if (ctp <= 4) z1p <- 0 else z1p <- 1
      if (is.element(ctp, c(2, 6))) xp <- z2p
      if (is.element(ctp, c(3, 7))) xp <- 1 - z2p
      if (is.element(ctp, c(1, 5))) xp <- 1
      if (is.element(ctp, c(4, 8))) xp <- 0

      df[z1 == z1p & x == xp & z2 == z2p, p := p + prob]
    }
  }

  df <- copy(df)
  return(df)
}

merge(pv(ufs_1), pv(ufs_3), by = c("z1", "z2", "x"))
