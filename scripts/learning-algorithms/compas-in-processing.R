
reticulate::use_python("/opt/homebrew/bin/python3", required = TRUE)
library(faircause)
library(ggplot2)
library(latex2exp)
root <- rprojroot::find_root(rprojroot::is_git_root)

set.seed(23)
data <- read.csv(file.path(root, "inst", "extdata", "compas-scores-two-years.csv"))
col.keep <- which(
  names(data) %in% c("age", "sex", "juv_fel_count",
                     "juv_misd_count", "juv_other_count", "priors_count",
                     "c_charge_degree", "race", "two_year_recid", "decile_score")
)
data <- data[, col.keep]
data$race <- factor(data$race)
levels(data$race) <- c("Minority", "Minority", "Majority", "Minority",
                       "Minority", "Minority")
data$race <- relevel(data$race, "Majority")
cumsum(table(data$decile_score)) / sum(table(data$decile_score))
## decile_score > 4 represents high risk (approximately)
data$northpointe <- as.integer(data$decile_score > 4)
data$decile_score <- NULL
names(data) <- gsub("_count", "", names(data))
names(data)[which(names(data) == "c_charge_degree")] <- "charge"

# remove any factors
data$sex <- ifelse(data$sex == "Male", 0, 1)
data$charge <- ifelse(data$sex == "M", 0, 1)
data$race <- ifelse(data$race == "Majority", 0, 1)

# construct the SFM
X <- "race"
Z <- c("age", "sex")
W <- c("juv_fel", "juv_misd", "juv_other", "priors", "charge")
Y <- c("two_year_recid")

# build our own predictor and decompose the true one
fair_pred <- fair_predictions(data, X, Z, W, Y, x0 = 0, x1 = 1,
                              BN = c("IE"))

preds <- predict(fair_pred, data, plot = TRUE)
data$faircause_preds <- preds$predictions[[5]]


# decompose the predictions on the eval set
train_idx <- seq_len(round(0.75 * nrow(data)))
faircause_decomp <- fairness_cookbook(data[-train_idx,], X = X, W = W, Z = Z, 
                                      Y = "faircause_preds",
                                      x0 = 0, x1 = 1, nboot1 = 20, nboot2=100)


# side-by-side plot of three decompositions
res <- rbind(
  cbind(fair_pred$y_meas, Outcome = "truth"),
  cbind(summary(faircause_decomp)$measures, Outcome = "faircause")
)

xlabz <- c(
  tv = TeX("$TV_{x_0, x_1}(y)$"),
  ctfde = TeX("$Ctf$-$DE_{x_0, x_1}(y | x_0)$"),
  ctfie = TeX("$Ctf$-$IE_{x_1, x_0}(y | x_0)$"),
  ctfse = TeX("$Ctf$-$SE_{x_1, x_0}(y)$")
)

# xlabz <- c(
#   tv = TeX("$TV_{x_0, x_1}(y)$"),
#   nde = TeX("$NDE_{x_0, x_1}(y | x_0)$"),
#   nie = TeX("$NIE_{x_1, x_0}(y | x_0)$"),
#   expse_x1 = TeX("$Exp$-$SE_{x_1}(y)$"),
#   expse_x0 = TeX("$Exp$-$SE_{x_0}(y)$")
# )

# res <- res[res$measure %in% c("ne", "nie", "expse_x1", "expse_x0", "tv"), ]
res <- res[res$measure %in% c("tv", "ctfde", "ctfse", "ctfie"), ]
res$measure <- as.factor(res$measure)

ggplot(res, aes(x = measure, y = value, fill = measure,
               color = Outcome, ymin = value - 2.58*sd, ymax = value + 2.58*sd)) +
  geom_bar(position="dodge", stat = "identity", linewidth = 1.5) +
  theme_minimal() +
  geom_errorbar(
    aes(group = Outcome),
    position = position_dodge(0.9),
    color = "black", width = 0.25
  ) +
  theme(
    legend.position = c(0.15, 0.8),
    legend.box.background = element_rect(),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    title = element_text(size = 16)
  ) + scale_x_discrete(labels = xlabz) +
  xlab("Causal Fairness Measure") +
  guides(fill="none") +
  scale_color_manual(values = c("red", "black")) +
  ggtitle("COMPAS Task 1 Analysis") +
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -0.1, ymax = 0.2),
            fill = "red", alpha = 0.01, linewidth = 0.05,
            linetype = "dotted") +
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = -0.1, ymax = 0.2),
            fill = "green", alpha = 0.01, linewidth = 0.05,
            linetype = "dotted") +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = -0.1, ymax = 0.2),
            fill = "green", alpha = 0.01, linewidth = 0.05,
            linetype = "dotted")

ggsave("~/Desktop/compas-in-processing.png", width = 10, height = 6)

