
reticulate::use_python("/opt/homebrew/bin/python3", required = TRUE)
library(faircause)
library(ggplot2)
root <- rprojroot::find_root(rprojroot::is_git_root)

# load the data
data <- read.csv(file.path(root, "scripts", "learning-algorithms",
                           "compas-preproc.csv"))[, -1]

train_idx <- 1:5000
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# construct the SFM mapping
X <- "race"
Z <- c("sex", "age")
W <- c("juv_fel", "juv_misd", "juv_other", "priors", "charge")
Y <- "two_year_recid"

# compute the decomposition to get \eta_1, \eta_2, \eta_3
fair_pred <- fair_predictions(train_data, X, Z, W, Y, x0 = 0, x1 = 1,
                              BN = c("SE", "IE"))

cowplot::plot_grid(
  autoplot(fair_pred, type = "causal"),
  autoplot(fair_pred, type = "accuracy"), ncol = 1L
)

# get predictions on test
test_preds <- predict(fair_pred, test_data)
autoplot(test_preds)
test_preds$predictions[[2]]
test_preds$plots
