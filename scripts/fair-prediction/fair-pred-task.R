library(readr)
library(rockchalk)
library(ranger)
library(ggplot2)
library(latex2exp)
library(stringr)
library(boot)

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

dataset <- "compas"

# load the data
dat <- get_data(dataset)

# load the metadata
mdat <- get_metadata(dataset)

# Method 1: Random Forest
org_dat <- dat
org_dat$two_year_recid <- ranger(two_year_recid ~ ., dat,
                                 classification = TRUE)$predictions

# Method 1: decompose Total Variation
org_tvd <- fairness_cookbook(org_dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]],
                             mdat[["Y"]], mdat[["x0"]], mdat[["x1"]])
org_plt <- autoplot(org_tvd, decompose = "xspec", dataaset = dataset) +
  xlab("") + ylab("")

# Method 2: reject-option classification
rjo_prb <- ranger(two_year_recid ~ ., dat, probability = TRUE)$predictions[, 2]
rjo_dat <- dat
rjo_dat$two_year_recid <- RejectOption(rjo_prb, rjo_dat$race)

rjo_tvd <- fairness_cookbook(rjo_dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]],
                             mdat[["Y"]], mdat[["x0"]], mdat[["x1"]])
rjo_plt <- autoplot(rjo_tvd, decompose = "xspec", dataset = dataset) +
  xlab("") + ylab("")


# Method 3: Reweighing by Kamiran & Calders
library(reticulate)
source_python(file.path(root, "py", paste0("reweighing_", dataset, ".py")))

Yhat_rew <- reweigh_and_predict(r_to_py(dat), r_to_py(dat))

reweigh_dat <- dat
reweigh_dat$two_year_recid <- as.vector(Yhat_rew)

reweigh_tvd <- fairness_cookbook(
  reweigh_dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]], mdat[["Y"]], mdat[["x0"]],
  mdat[["x1"]])

rew_plt <- autoplot(reweigh_tvd, decompose = "xspec", dataset = dataset) +
  xlab("") + ylab("")


# Method 4: Reductions (Agarwal et. al.)
source_python(file.path(root, "py", paste0("reductions_", dataset, ".py")))

red_dat <- dat
Yhat_red <- reduce_and_predict(r_to_py(dat), r_to_py(dat), 0.01)
red_dat$two_year_recid <- as.vector(Yhat_red)
red_tvd <- fairness_cookbook(
  red_dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]], mdat[["Y"]], mdat[["x0"]],
  mdat[["x1"]]
)

red_plt <- autoplot(red_tvd, decompose = "xspec", dataset = dataset) +
  xlab("") + ylab("")

multi_plot(
  org_plt +
    ggtitle(TeX("$TV_{x_0, x_1}(\\hat{y})$ decomposition: Random Forest on COMPAS")),
  rew_plt +
    ggtitle(TeX("$TV_{x_0, x_1}(\\hat{y})$ decomposition: Reweighing on COMPAS")),
  red_plt +
    ggtitle(TeX("$TV_{x_0, x_1}(\\hat{y})$ decomposition: Reductions on COMPAS")),
  rjo_plt +
    ggtitle(TeX("$TV_{x_0, x_1}(\\hat{y})$ decomposition: Reject-option on COMPAS")),
  labels = c("(i)", "(ii)", "(iii)", "(iv)"), ncol = 2L
)

scale_factor <- 14
ggsave(file.path(root, "misc", "paper", "figures", "compas-fairpred.png"),
       width = 4/3*scale_factor, height = scale_factor, bg = "white")


# Fairadapt Plecko & Meinshausen
# load the adjacency matrix
# set.seed(2022)
# mats <- get_mat(dataset)
# adj.mat <- mats[[1L]]
# cfd.mat <- mats[[2L]]
#
# fdp <- fairadapt::fairadapt(two_year_recid ~ ., prot.attr = "race",
#                             train.data = dat, adj.mat = adj.mat)
#
# # inspect probability densities
# autoplot(fdp)
# ad_dat <- fairadapt:::adaptedData(fdp)
# ad_dat$race <- dat$race
#
# covar <- "juv_fel_count"
# cowplot::plot_grid(
#   ggplot(ad_dat, aes_string(x = covar, fill = "race")) + theme_bw() +
#     geom_density(alpha = 0.4) + ggtitle("Adapted"),
#   ggplot(dat, aes_string(x = covar, fill = "race")) + theme_bw() +
#     geom_density(alpha = 0.4) + ggtitle("Original"), ncol = 2L
# )
#
# adapt_oob <- ranger(two_year_recid ~ ., ad_dat,
#                     classification = TRUE)$predictions
# ad_dat$two_year_recid <- adapt_oob
#
#
# dat.fairadapt <- dat
# dat.fairadapt$two_year_recid <- adapt_oob
#
# fairadapt_tvd <- fairness_cookbook(
#   ad_dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]],
#   mdat[["Y"]], mdat[["x0"]], mdat[["x1"]]
# )
#
# fairadapt_plt <- autoplot(fairadapt_tvd, decompose = "xspec", dataset = dataset) +
#   xlab("") + ylab("")
#
# fairadapt_plt
# scale_factor <- 8
# ggsave(file.path(root, "paper", "figures", "compas-fairadapt.png"),
#        width = 4/3*scale_factor, height = scale_factor, bg = "white")
