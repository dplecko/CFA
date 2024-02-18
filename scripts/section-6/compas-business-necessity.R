
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
invisible(lapply(list.files(file.path(root, "scripts", "helpers"),
                            full.names = TRUE), source))

dataset <- "compas"
dat <- faircause::compas
mdat <- get_metadata(dataset)

fc_compas <- fairness_cookbook(dat, X = mdat$X, Z = mdat$Z, W = mdat$W,
                               Y = mdat$Y, x0 = mdat$x0, x1 = mdat$x1,
                               model = "linear")

cat("Ctf-IE = ", spec_dec(100 * fc_compas$measures$CtfIE[1], 1), "\\%",
    " \\pm ", spec_dec(100 * fc_compas$measures$CtfIE[2], 1), "\\%",  sep = "")

cat("Ctf-SE = ", spec_dec(100 * fc_compas$measures$CtfSE[1], 1), "\\%",
    " \\pm ", spec_dec(100 * fc_compas$measures$CtfSE[2], 1), "\\%",  sep = "")

autoplot(fc_compas, decompose = "xspec", dataset = "COMPAS")

# measuring the BN effects
est_quant <- function(data, xA, xB, xC, n_samp = 10000, seed = 22) {

  # sample P(Z1)
  age <- sample(data$age[data$race %in% xA], size = n_samp, replace = TRUE)

  est_dat <- data.frame(age = age)

  # fit the Z2 | Z1, X models
  if (is.element(xB, levels(data$race))) {
    mod_one <- glm(sex ~ age + race, data = data, family = "binomial")
  } else {
    mod_one <- glm(sex ~ age, data = data, family = "binomial")
  }

  est_dat$race <- xB
  if (is.element(xB, levels(data$race)))
    levels(est_dat$race) <- levels(data$race)

  est_dat <- cbind(est_dat, sex = predict(mod_one, est_dat, type = "response"))
  est_dat$sex <- factor(ifelse(rbinom(n_samp, size = 1, prob = est_dat$sex),
                               "Male", "Female"))

  # fit the Y | Z1, Z2, X models
  logreg <- glm(two_year_recid ~ age + sex + race, data = data,
                family = "binomial")

  est_dat$race <- xC
  levels(est_dat$race) <- levels(data$race)
  est_dat$two_year_recid <- rbinom(n_samp, size = 1,
                                   prob = predict(logreg, est_dat,
                                                  type = "response"))

  mean(est_dat$two_year_recid)
}

# e1 <- est_quant(dat, "White", "White", "White", nrow(dat))
# e2 <- est_quant(dat, c("White", "Non-White"), "White", "White", nrow(dat))
# e3 <- est_quant(dat, c("White", "Non-White"), "", "White", nrow(dat))
#
# cat("Exp-SE_x1 =", 100 * fc_compas$measures$ExpSE_x0[1], "\n")
# cat("Exp-SE_x1 =", 100 * (e1-e3), "(alternative) \n")
# cat("Z1 contribution = ", 100 * (e1-e2), "\n")
# cat("Z2 contribution = ", 100 * (e2-e3), "\n")

# continue to Ctf-SE -- think about indirect
nrep <- 100
c1 <- sapply(seq_len(nrep), function(i) est_quant(dat, "White", "White",
                                                  "Non-White", nrow(dat), i))
c2 <- sapply(seq_len(nrep), function(i) est_quant(dat, "White", "Non-White",
                                                  "Non-White", nrow(dat), i))
c3 <- sapply(seq_len(nrep), function(i) est_quant(dat, "Non-White", "Non-White",
                                                  "Non-White", nrow(dat), i))

cat("Ctf-SE_{x1,x0} =", 100 * fc_compas$measures$CtfSE[1], "\n")
cat("Ctf-SE_{x1,x0} =", round(100 * mean(c1-c3), 1), "+/-",
    round(100 * sd(c1-c3), 1), "(alternative) \n")
cat("gender contribution =", round(100 * mean(c1-c2), 1), "+/-",
    round(100 * sd(c1-c2), 1), "\n")
cat("age contribution =", round(100 * mean(c2-c3), 1), "+/-",
    round(100 * sd(c2-c3), 1), "\n")

