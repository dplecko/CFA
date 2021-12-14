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


dataset <- "berkeley"

# Step 1 - load the data
dat <- get_data(dataset)

# Step 2 - load the metadata
mdat <- get_metadata(dataset)

# Step 3 - decompose Total Variation
tvd <- CausalExplanation_TV(dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]], 
                            mdat[["Y"]], mdat[["x0"]], mdat[["x1"]],
                            probability = 
                              (length(unique(dat[[mdat[["Y"]]]])) == 2L))

TV_family(tvd, dataset)

ggsave(file.path(root, "paper", "figures", paste0(dataset, ".png")),
       width = 16, height = 8)
