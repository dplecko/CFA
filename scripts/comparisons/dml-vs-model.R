library(readr)
library(rockchalk)
library(ranger)
library(ggplot2)
library(latex2exp)
library(stringr)

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))


dataset <- "census"

# Step 1 - load the data
dat <- get_data(dataset)

# Step 2 - load the metadata
mdat <- get_metadata(dataset)

# Step 3 - decompose Total Variation
tvd <- CausalExplanation_TV(dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]], 
                            mdat[["Y"]], mdat[["x0"]], mdat[["x1"]])

DR_tvd <- DoubleRobustCausalExpTV(dat, mdat[["X"]], mdat[["W"]], mdat[["Z"]], 
                                  mdat[["Y"]], mdat[["x0"]], mdat[["x1"]])

compare_methods <- rbind(
  data.frame(measure = names(tvd), value = unlist(tvd), method = "Model"),
  data.frame(measure = names(DR_tvd), value = unlist(DR_tvd), method = "DML")
)

ggplot(compare_methods, aes(x = measure, y = value, fill = method)) +
  geom_col(position = "dodge") + theme_minimal() +
  ggtitle("Comparison of model-based and DML") +
  theme(
    legend.position = c(0.2, 0.8),
    legend.box.background = element_rect()
  )
