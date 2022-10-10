
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

get_ie <- function(data, n_samp, spec, seed = 22) {

  set.seed(seed)
  # sample P(Z1)
  xA <- spec[[1]][["xA"]]
  age <- sample(data$age[data$race %in% xA], size = n_samp, replace = TRUE)
  est_dat <- data.frame(age = age)

  for (i in seq_along(spec)[-1]) {

    # get formula
    covs <- paste( spec[[i]][["covs"]], collapse = "+")
    form <- as.formula(paste(spec[[i]][["response"]], "~", covs))

    # fit model on original data
    binary <- length(unique(data[[spec[[i]][["response"]]]])) == 2L
    if (binary) {
      model <- glm(form, data, family = "binomial")
    } else {
      model <- lm(form, data)
    }

    # set the desired xlvl for this stage
    est_dat$race <- spec[[i]][["xlvl"]]
    levels(est_dat$race) <- levels(data$race)

    # sample the adjusted data
    new_col <- predict(model, est_dat, type = "response")
    if (binary) {
      new_col <- rbinom(length(new_col), size = 1, prob = new_col)
      if (is.factor(data[[spec[[i]][["response"]]]])) {
        lvls <- levels(data[[spec[[i]][["response"]]]])
        new_col <- factor(ifelse(new_col, lvls[2], lvls[1]))
      }
    }
    est_dat <- cbind(est_dat, new_col)
    names(est_dat)[i+1] <- spec[[i]][["response"]]

  }

  mean(est_dat$two_year_recid)
}

x0 <- "White"
x1 <- "Non-White"

spec_x1wx0<- list(
  age = list(xA = x0),
  sex = list(response = "sex", covs = c("age", "race"), xlvl = x0),
  juv_fel_count = list(response = "juv_fel_count", covs = c("age", "race", "sex"),
                       xlvl = x0),
  juv_misd_count = list(response = "juv_misd_count", covs = c("age", "race", "sex"),
                        xlvl = x0),
  juv_other_count = list(response = "juv_other_count", covs = c("age", "race", "sex"),
                         xlvl = x0),
  priors_count = list(response = "priors_count",
                      covs = c("age", "race", "sex", "juv_fel_count",
                               "juv_misd_count", "juv_other_count"),
                      xlvl = x0),
  c_charge_degree = list(response = "c_charge_degree",
                      covs = c("age", "race", "sex", "juv_fel_count",
                               "juv_misd_count", "juv_other_count",
                               "priors_count"),
                      xlvl = x0),
  two_year_recid = list(response = "two_year_recid",
                         covs = c("age", "race", "sex", "juv_fel_count",
                                  "juv_misd_count", "juv_other_count",
                                  "priors_count", "c_charge_degree"),
                         xlvl = x1)
)

spec_x1wx1<- list(
  age = list(xA = x0),
  sex = list(response = "sex", covs = c("age", "race"), xlvl = x0),
  juv_fel_count = list(response = "juv_fel_count", covs = c("age", "race", "sex"),
                       xlvl = x1),
  juv_misd_count = list(response = "juv_misd_count", covs = c("age", "race", "sex"),
                        xlvl = x1),
  juv_other_count = list(response = "juv_other_count", covs = c("age", "race", "sex"),
                         xlvl = x1),
  priors_count = list(response = "priors_count",
                      covs = c("age", "race", "sex", "juv_fel_count",
                               "juv_misd_count", "juv_other_count"),
                      xlvl = x1),
  c_charge_degree = list(response = "c_charge_degree",
                         covs = c("age", "race", "sex", "juv_fel_count",
                                  "juv_misd_count", "juv_other_count",
                                  "priors_count"),
                         xlvl = x1),
  two_year_recid = list(response = "two_year_recid",
                        covs = c("age", "race", "sex", "juv_fel_count",
                                 "juv_misd_count", "juv_other_count",
                                 "priors_count", "c_charge_degree"),
                        xlvl = x1)
)

spec_x1wx1_jx0 <- list(
  age = list(xA = x0),
  sex = list(response = "sex", covs = c("age", "race"), xlvl = x0),
  juv_fel_count = list(response = "juv_fel_count",
                       covs = c("age", "race", "sex"),
                       xlvl = x0),
  juv_misd_count = list(response = "juv_misd_count",
                        covs = c("age", "race", "sex"),
                        xlvl = x0),
  juv_other_count = list(response = "juv_other_count",
                         covs = c("age", "race", "sex"),
                         xlvl = x0),
  priors_count = list(response = "priors_count",
                      covs = c("age", "race", "sex", "juv_fel_count",
                               "juv_misd_count", "juv_other_count"),
                      xlvl = x1),
  c_charge_degree = list(response = "c_charge_degree",
                         covs = c("age", "race", "sex", "juv_fel_count",
                                  "juv_misd_count", "juv_other_count",
                                  "priors_count"),
                         xlvl = x1),
  two_year_recid = list(response = "two_year_recid",
                        covs = c("age", "race", "sex", "juv_fel_count",
                                 "juv_misd_count", "juv_other_count",
                                 "priors_count", "c_charge_degree"),
                        xlvl = x1)
)

spec_x1wx1_pdx0 <- list(
  age = list(xA = x0),
  sex = list(response = "sex", covs = c("age", "race"), xlvl = x0),
  juv_fel_count = list(response = "juv_fel_count",
                       covs = c("age", "race", "sex"),
                       xlvl = x1),
  juv_misd_count = list(response = "juv_misd_count",
                        covs = c("age", "race", "sex"),
                        xlvl = x1),
  juv_other_count = list(response = "juv_other_count",
                         covs = c("age", "race", "sex"),
                         xlvl = x1),
  priors_count = list(response = "priors_count",
                      covs = c("age", "race", "sex", "juv_fel_count",
                               "juv_misd_count", "juv_other_count"),
                      xlvl = x0),
  c_charge_degree = list(response = "c_charge_degree",
                         covs = c("age", "race", "sex", "juv_fel_count",
                                  "juv_misd_count", "juv_other_count",
                                  "priors_count"),
                         xlvl = x0),
  two_year_recid = list(response = "two_year_recid",
                        covs = c("age", "race", "sex", "juv_fel_count",
                                 "juv_misd_count", "juv_other_count",
                                 "priors_count", "c_charge_degree"),
                        xlvl = x1)
)

nrep <- 100

i1 <- sapply(seq_len(nrep), function(i) get_ie(dat, n_samp = nrow(dat),
                                               spec = spec_x1wx0, seed = i))
i2 <- sapply(seq_len(nrep), function(i) get_ie(dat, n_samp = nrow(dat),
                                               spec = spec_x1wx1_jx0, seed = i))
i3 <- sapply(seq_len(nrep), function(i) get_ie(dat, n_samp = nrow(dat),
                                               spec = spec_x1wx1, seed = i))

i4 <- sapply(seq_len(nrep), function(i) get_ie(dat, n_samp = nrow(dat),
                                               spec = spec_x1wx1_pdx0, seed = i))


cat("Ctf-IE_{x1,x0}(y | x0) =", round(100 * mean(i1-i3), 1), "+/-",
    round(100 * sd(i1-i3), 1), "\n")
cat("juvenile contribution =", round(100 * mean(i1-i2), 1), "+/-",
    round(100 * sd(i1-i2), 1), "\n")
cat("priors/degree contribution =", round(100 * mean(i2-i3), 1), "+/-",
    round(100 * sd(i2-i3), 1), "\n")

cat("---- Alternative options (symmetric) ----\n")
cat("juvenile contribution =", round(100 * mean(i1-i4), 1), "+/-",
    round(100 * sd(i1-i4), 1), "\n")
cat("priors/degree contribution =", round(100 * mean(i4-i3), 1), "+/-",
    round(100 * sd(i4-i3), 1), "\n")


# cat("Ctf-IE_{x1,x0}(y | x0) =", 100 * fc_compas$measures$CtfIE[1], "\n")
# cat("Ctf-IE_{x1,x0}(y | x0) =", 100 * (i1-i3), "(alternative) \n")
# cat("juvenile contribution =", 100 * (i1-i2), "\n")
# cat("priors/degree contribution =", 100 * (i2-i3), "\n")

# for tomorrow -- check the above -- consider replacing with non-parametric learners (RF)
# can also utilize previous mod_predict mod_train from the faircause package


