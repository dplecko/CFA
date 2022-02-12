library(readr)
library(rockchalk)
library(ranger)
library(ggplot2)
library(latex2exp)
library(stringr)
library(assertthat)

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))


dataset <- "census"

# Step 1 - load the data
dat <- get_data(dataset)

# Step 2 - load the metadata
mdat <- get_metadata(dataset)

# Step 3 - decompose Total Variation
tvd <- CausalExplanation_TV(dat, X = mdat[["X"]], W = mdat[["W"]], 
                            Z = mdat[["Z"]], Y = mdat[["Y"]], x0 = mdat[["x0"]],
                            x1 = mdat[["x1"]])

# Step 4 - decompose Equality of odds (if classification)
eod <- CausalExplanation_EO(dat, X = mdat[["X"]], W = mdat[["W"]], 
                            Z = mdat[["Z"]], Y = mdat[["Y"]], x0 = mdat[["x0"]],
                            x1 = mdat[["x1"]], mdat[["ylvl"]])

# Step 5 - visualize the results
{
  
  df <- data.frame(names(tvd), unlist(tvd))
  names(df) <- c("Measure", "Value")
  
  x_tikz <- c(TeX("DE_{x_0, x_1}(y | x_0)", output = "character"), 
              TeX("ETT_{x_0, x_1}(y)", output = "character"), 
              TeX("IE_{x_1, x_0}(y | x_0)", output = "character"),
              TeX("SE_{x_1, x_0}(y)", output = "character"), 
              TeX("TV_{x_0, x_1}(y)", output = "character"))
  
  tv_plot <- ggplot(df, aes(x = Measure, y = Value, fill = Measure)) + geom_col() +
    theme_minimal() + 
    theme(
      legend.position = "none",
      axis.text = element_text(size = 15),
      axis.title.x = element_text(size = 15),
      title = element_text(size = 18)
    ) + scale_x_discrete(labels = parse(text = x_tikz)) + 
    xlab("Counterfactual Measure") + 
    ggtitle(TeX(paste0("Decomposition of total variation TV_{x_0, x_1}(y): ",
                       str_to_title(dataset), " dataset")))
  
  
  df <- data.frame(names(eod), unlist(eod))
  names(df) <- c("Measure", "Value")
  
  x_tikz <- c(TeX("ER_{x_0, x_1}($\\hat{y}$ | y)", "text"), 
              TeX("ER^d_{x_0, x_1}($\\hat{y}$ | x_0, y)", "text"), 
              TeX("ER^i_{x_1, x_0}($\\hat{y}$ | x_0, y)", "text"),
              TeX("ER^s_{x_1, x_0}($\\hat{y}$ | y)", "text"))
  
  eo_plot <- ggplot(df, aes(x = Measure, y = Value, fill = Measure)) + geom_col() +
    theme_minimal() + 
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 12)
    ) + scale_x_discrete(labels = parse(text = x_tikz)) + 
    xlab("Counterfactual Measure") + 
    ggtitle(TeX(
      paste0("Decomposition of equalized odds ER_{x_0, x_1}($\\hat{y}$): ",
             str_to_sentence(dataset), " dataset")))
  
  cowplot::plot_grid(
    tv_plot, eo_plot, ncol = 2L, labels = c("A", "B")
  )
  
  
}

# tv_plot + ggtitle(TeX(paste0("Decomposition of total variation TV_{x_0, x_1}(y):
#                               on 2018 US Census dataset")))
# ggsave("~/fairness/CFA/paper/census.png", height = 6, width = 12)
