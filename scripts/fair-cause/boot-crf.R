
# Load necessary libraries
library(grf)
library(data.table)

#' * Heterogeneity of (z, w)-DE_{x_0, x_1} * 

# Retrieve command-line arguments
args <- commandArgs(trailingOnly = TRUE)
i <- as.integer(args[1])
data_path <- args[2]
X <- unlist(strsplit(args[3], ","))
W <- args[4]
Y <- args[5]
output_path <- args[6]

# Load data
dat <- fread(data_path)

# Set seed
set.seed(i)

# Perform bootstrap iteration
n <- nrow(dat)
in_bag <- sample(1:n, size = n, replace = TRUE)
out_bag <- setdiff(1:n, in_bag)

crf <- causal_forest(
  X = as.matrix(dat[in_bag, X, with = FALSE]),
  W = dat[in_bag][[W]], Y = dat[in_bag][[Y]]
)

# out-of-bag
preds <- predict(
  crf, as.matrix(dat[out_bag, X, with = FALSE])
)$predictions

# in-bag oob predictions
preds <- c(preds, crf$predictions[, 1])

# Save predictions to file
write.csv(unique(data.table(id = c(out_bag, in_bag), pred = preds)), 
          file = output_path, row.names = FALSE)
