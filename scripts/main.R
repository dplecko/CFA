
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "R")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

dataset <- "compas"

# Step 1 - load the data
dat <- get_data(dataset)

# Step 2 - load the metadata
mdat <- get_metadata(dataset)

# Step 3 - decompose Total Variation
tvd <- fairness_cookbook(dat, X = mdat[["X"]], W = mdat[["W"]], 
                         Z = mdat[["Z"]], Y = mdat[["Y"]], x0 = mdat[["x0"]],
                         x1 = mdat[["x1"]])

# Step 4 - visualize the results
autoplot(tvd)

# tv_plot + ggtitle(TeX(paste0("Decomposition of total variation TV_{x_0, x_1}(y):
#                               on 2018 US Census dataset")))
# ggsave("~/fairness/CFA/paper/census.png", height = 6, width = 12)
