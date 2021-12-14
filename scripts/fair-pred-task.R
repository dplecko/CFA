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
org_tvd <- CausalExplanation_TV(
  org_dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]], 
  mdat[["Y"]], mdat[["x0"]], mdat[["x1"]],
  probability = (length(unique(dat[[mdat[["Y"]]]])) == 2L)
)
org_plt <- TV_family(org_tvd, dataset)

# Method 2: reject-option classification
prob <- ranger(two_year_recid ~ ., dat, probability = TRUE)$predictions[, 2]
rejopt_dat <- dat
rejopt_dat$two_year_recid <- RejectOption(prob, rejopt_dat$race)

rejopt_tvd <- CausalExplanation_TV(
  rejopt_dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]], 
  mdat[["Y"]], mdat[["x0"]], mdat[["x1"]],
  probability = (length(unique(dat[[mdat[["Y"]]]])) == 2L)
)
rejopt_plt <- TV_family(rejopt_tvd, dataset)


# Method 3: Reweighing by Kamiran & Calders
# Sys.setenv(RETICULATE_PATH = "/usr/local/bin/python3")
# install.packages("reticulate")
reticulate::use_condaenv("r-reticulate")
# reticulate::py_config()
library(reticulate)

source_python(file.path(root, "py", paste0("reweighing_", dataset, ".py")))

Y.hat <- reweigh_and_predict(r_to_py(dat), r_to_py(dat))

reweigh_dat <- dat
reweigh_dat$two_year_recid <- as.vector(Y.hat)

reweigh_tvd <- CausalExplanation_TV(
  reweigh_dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]], mdat[["Y"]], mdat[["x0"]],
  mdat[["x1"]], probability = (length(unique(dat[[mdat[["Y"]]]])) == 2L))

reweigh_plt <- TV_family(reweigh_tvd, dataset)


# Method 4: Reductions (Agarwal et. al.)
source_python(file.path(root, "py", paste0("reductions_", dataset, ".py")))

red_dat <- dat
red_dat$two_year_recid <- reduce_and_predict(r_to_py(dat), r_to_py(dat), 0.01)
red_tvd <- CausalExplanation_TV(
  red_dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]], mdat[["Y"]], mdat[["x0"]],
  mdat[["x1"]], probability = (length(unique(dat[[mdat[["Y"]]]])) == 2L))

red_plt <- TV_family(red_tvd, dataset)

multi_plot(
  org_plt + 
    ggtitle(TeX("TV_{x_0, x_1}(\\hat{y}) decomposition: Random Forest on COMPAS")), 
  reweigh_plt +
    ggtitle(TeX("TV_{x_0, x_1}(\\hat{y}) decomposition: Reweighing on COMPAS")),
  red_plt +
    ggtitle(TeX("TV_{x_0, x_1}(\\hat{y}) decomposition: Reductions on COMPAS")),
  rejopt_plt +
    ggtitle(TeX("TV_{x_0, x_1}(\\hat{y}) decomposition: Reject-option on COMPAS")),
  labels = c("(i)", "(ii)", "(iii)", "(iv)"), ncol = 2L
)

scale_factor <- 22
ggsave(file.path(root, "paper", "figures", "compas-fairpred.png"),
       width = scale_factor, height = scale_factor)


# Fairadapt Plecko & Meinshausen
# load the adjacency matrix
mats <- get_mat(dataset)
adj.mat <- mats[[1L]]
cfd.mat <- mats[[2L]]

fdp <- fairadapt::fairadapt(two_year_recid ~ ., prot.attr = "race",
                            train.data = dat, adj.mat = adj.mat)#,
                            #cfd.mat = cfd.mat)

# inspect probability densities
autoplot(fdp)
ad_dat <- fairadapt:::adaptedData(fdp)
ad_dat$race <- dat$race

covar <- "juv_fel_count"
cowplot::plot_grid(
  ggplot(ad_dat, aes_string(x = covar, fill = "race")) + theme_bw() +
    geom_density(alpha = 0.4) + ggtitle("Adapted"),
  ggplot(dat, aes_string(x = covar, fill = "race")) + theme_bw() +
    geom_density(alpha = 0.4) + ggtitle("Original"), ncol = 2L
)

adapt.oob <- ranger(two_year_recid ~ ., ad_dat,
                    classification = TRUE)$predictions
ad_dat$two_year_recid <- adapt.oob


dat.fairadapt <- dat
dat.fairadapt$two_year_recid <- adapt.oob

fairadapt_tvd <- CausalExplanation_TV(
  ad_dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]],
  mdat[["Y"]], mdat[["x0"]], mdat[["x1"]],
  probability = (length(unique(dat[[mdat[["Y"]]]])) == 2L))

fairadapt_plt <- TV_family(fairadapt_tvd, dataset)

multi_plot(org_plt, fairadapt_plt)

################################################################################

# for reductions (Agarwal et. al.)

# source_python(file.path(root, "py", paste0("reductions_", dataset, ".py")))
#py_run_string("import importlib")
#py_run_string("moments = importlib.reload(moments)")
#py_run_string("red = importlib.reload(red)")

# else if (method == 5) {
#   # Reductions approach Agarwal et. al.
#   Y.hat <- reduce_and_predict(r_to_py(train.data), r_to_py(test.data), 0.001)
# }
# else if (method == 6) {
#   # Reductions approach Agarwal et. al.
#   Y.hat <- reduce_and_predict(r_to_py(train.data), r_to_py(test.data), 0.01)
# }

# ggsave(file.path(root, "paper", "figures", paste0(dataset, ".png")),
#        width = 16, height = 8)
