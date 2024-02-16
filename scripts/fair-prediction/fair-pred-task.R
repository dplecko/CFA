library(readr)
library(rockchalk)
library(ranger)
library(ggplot2)
library(latex2exp)
library(stringr)
library(boot)


# aliasing the autplot.faircause
autoplot.faircause <- function(x, decompose = c("xspec", "general", "both"),
                               dataset = "", signed = TRUE, ...) {

  decompose <- match.arg(decompose, c("xspec", "general", "both"))
  df <- summarize_measures(x$measures)
  names(df) <- c("Measure", "Value", "StdDev")

  rename <- list(
    tv = TeX("$TV_{x_0, x_1}(y)$"),
    te = TeX("$TE_{x_0, x_1}(y)$"),
    expse_x1 = TeX("$Exp$-SE_{x_1}(y)$"),
    expse_x0 = TeX("$Exp$-SE_{x_0}(y)$"),
    nde = TeX("$NDE_{x_0, x_1}(y)$"),
    nie = TeX("$NIE_{x_1, x_0}(y)$"),
    ett = TeX("$ETT_{x_0, x_1}(y | x_0)$"),
    ctfde = TeX("$Ctf$-$DE_{x_0, x_1}(y | x_0)$"),
    ctfie = TeX("$Ctf$-$IE_{x_1, x_0}(y | x_0)$"),
    ctfse = TeX("$Ctf$-$SE_{x_1, x_0}(y)$")
  )
  ttl <- "$TV_{x_0, x_1}(y)$"

  if (!signed) {

    assertthat::assert_that(
      decompose == "xspec",
      msg = "Signed = TRUE not supported for decompose != xspec."
    )
    rename$TV <- ifelse(x$eo, TeX("$ER_{x_0, x_1}(\\hat{y} | y)$"),
                        TeX("$PG_{x_0, x_1}(y)$"))
    ttl <- ifelse(x$eo, "$ER_{x_0, x_1}(\\hat{y} | y)$", "$PG_{x_0, x_1}(y)$")
    rename$CtfDE <- "Direct"
    rename$CtfIE <- "Indirect"
    rename$CtfSE <- "Confounded"

    sgn_idx <- which(df$Measure %in% c("ctfie", "ctfse"))

    df$Value[sgn_idx] <- df$Value[sgn_idx] * (-1)

  } else if (x$eo) {

    ttl <- "$ER_{x_0, x_1}(\\hat{y} | y)$"
    assertthat::assert_that(
      decompose == "xspec",
      msg = "eo = TRUE only supported for decompose == xspec."
    )
    rename$CtfDE <- TeX("$ER^{de}_{x_0, x_1}(\\hat{y} | y, x_0)$")
    rename$CtfIE <- TeX("$ER^{ie}_{x_1, x_0}(\\hat{y} | y, x_0)$")
    rename$CtfSE <- TeX("$ER^{se}_{x_0, x_1}(\\hat{y} | y)$")
  }

  inc_meas <- switch(
    decompose,
    xspec = c("tv", "ctfde", "ctfie", "ctfse"),
    general = c("tv", "nde", "nie", "expse_x0", "expse_x1"),
    both = names(rename)
  )

  df$Measure <- factor(df$Measure, levels = names(rename))
  df <- df[df$Measure %in% inc_meas, ]

  xlabz <- parse(text = unlist(rename[names(rename) %in% df$Measure]))

  ggplot(df, aes(x = Measure, y = Value, fill = Measure)) + geom_col() +
    theme_minimal() + cowplot::theme_minimal_hgrid() +
    geom_errorbar(
      aes(x = Measure, ymin = Value - 1.96*StdDev, ymax = Value + 1.96*StdDev),
      color = "black", width = 0.5
    ) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 14),
      axis.title.x = element_text(size = 18),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
      title = element_text(size = 18)
    ) + scale_x_discrete(labels = xlabz) +
    xlab("Causal Fairness Measure") +
    ggtitle(TeX(paste0(ttl, " decomposition ", dataset)))
}

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
invisible(lapply(list.files(file.path(root, "scripts", "helpers"),
                            full.names = TRUE), source))

dataset <- "compas"

# load the data
dat <- get(data("compas", package = "faircause"))

# load the metadata
mdat <- SFM_proj(dataset)

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
  org_plt + geom_vline(xintercept = 1.5, linetype = "dotted") +
    ggtitle("Random Forest"),
  rew_plt + geom_vline(xintercept = 1.5, linetype = "dotted") + ggtitle("Reweighing"),
  red_plt + geom_vline(xintercept = 1.5, linetype = "dotted") + ggtitle("Reductions"),
  rjo_plt + geom_vline(xintercept = 1.5, linetype = "dotted") + ggtitle("Reject-option"),
  labels = c("(i)", "(ii)", "(iii)", "(iv)"), ncol = 2L
)

scale_factor <- 10
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
