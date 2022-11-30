
library(faircause)
library(ggplot2)
library(reshape2)
library(ranger)

root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
invisible(lapply(list.files(file.path(root, "r"), full.names = TRUE), source))

col_adm <- function(n, cfs, kap = cfs[["kap"]], lam = cfs[["lam"]],
                    alf = cfs[["alf"]], bet = cfs[["bet"]]) {

  u_xz <- rbinom(n, size = 1, prob = 0.5)
  x <- rbinom(n, size = 1, prob = 0.5 + 0.1 * u_xz)
  z <- rbinom(n, size = 1, prob = 0.5 + kap * u_xz)
  d <- rbinom(n, size = 1, prob = 0.5 + lam * x)
  y <- rbinom(n, size = 1, prob = 0.1 + alf * x + bet * d + 0.1 * z)

  data.frame(x, z, d, y)

}

cfs_nxt <- function(cfs, kap = cfs[["kap"]], lam = cfs[["lam"]],
                    alf = cfs[["alf"]], bet = cfs[["bet"]]) {

  kapt <- kap * 0.9
  lamt <- lam * (1 - bet)
  bett <- bet * (1 - lam) * runif(1, 0.8, 1.2)
  alft <- alf * 0.8

  list(kap = kapt, lam = lamt, alf = alft, bet = bett)

}

col_adm_tim <- function(n, cfs) {

  dat <- bqn <- gtr <- list()

  for (t in seq_len(10)) {

    # generate data
    dat[[t]] <- col_adm(n, cfs)

    # compute decomposition
    bqn[[t]] <- fairness_cookbook(dat[[t]], X = "x", Z = "z", W = "d", Y = "y",
                                  x0 = 0, x1 = 1)

    # get ground truth
    gtr[[t]] <- c(cfs$alf, -(cfs$bet * cfs$lam),
                  0.1 * ( (0.125 + 0.2 * (0.5 + cfs$kap))/(0.45) -
                             (0.125 + 0.4 * (0.5 + cfs$kap))/(0.55) ))

    # update the coefficients
    cfs <- cfs_nxt(cfs)

  }

  list(dat = dat, bqn = bqn, gtr = gtr)

}

cfs <- list(kap = 0.3, lam = 0.2, alf = 0.1, bet = 0.3)

set.seed(22)
res <- col_adm_tim(n = 5000L, cfs = cfs)

otp <- c()
for (t in seq_along(res[["bqn"]])) {

  x <- res[["bqn"]][[t]]
  x_tr <- res[["gtr"]][[t]]
  add_row <- data.frame(Spurious = x$measures$CtfSE[1],
                        Indirect = x$measures$CtfIE[1],
                        Direct = x$measures$CtfDE[1],
                        Spurious_True = x_tr[3],
                        Indirect_True = x_tr[2],
                        Direct_True = x_tr[1],
                        Year = 2010+t)

  otp <- rbind(otp, add_row)

}

df <- reshape2::melt(otp, id.vars = "Year", variable.name = "Measure")
df$Meas <- gsub("_.*", "", df$Measure)
df$whc <- ifelse(grepl("_", df$Measure), "True (population)",
                 "Estimated (from sample)")

ggplot(df, aes(x = Year, y = value, color = Meas, linetype = whc)) +
  geom_line() + geom_point() + theme_bw() +
  ggtitle("Bias Quantification Over Time - College Admissions") +
  ylab("Value") + scale_y_continuous(labels = scales::percent,
                                     limits = c(-0.1, 0.15)) +
  scale_x_continuous(breaks = seq(2010, 2020)) +
  theme(
    legend.position = "bottom",
    legend.box.background = element_rect(),
    legend.box = "horizontal"
  ) + scale_linetype_discrete(name = "Quantity") +
  scale_color_discrete(name = "Effect")

ggsave(file.path(root, "misc", "paper", "figures", "col-adm-time.png"),
       height = 5, width = 8)


