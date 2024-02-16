root <- rprojroot::find_root(rprojroot::is_git_root)

library(fairadapt)
library(faircause)
library(ranger)
library(ggplot2)
library(latex2exp)

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

# decomposing the true outcome
X <- "race"
Z <- c("age", "sex")
W <- c("juv_fel", "juv_misd", "juv_other", "priors", "charge")
Y <- c("two_year_recid")
two_year <- fairness_cookbook(data, X = X, W = W, Z = Z, Y = Y,
                              x0 = "Majority", x1 = "Minority", nboot1 = 5)

# decomposing the Northpointe predictions
Yhat <- "northpointe"
northpointe <- fairness_cookbook(data, X = X, W = W, Z = Z, Y = Yhat,
                                 x0 = "Majority", x1 = "Minority", nboot1 = 5)

# side-by-side plot of three decompositions
res <- rbind(
  cbind(summary(two_year)$measures, outcome = "true"),
  cbind(summary(northpointe)$measures, outcome = "northpointe")
)


res <- res[res$measure %in% c("ctfde", "ctfie", "ctfse", "tv", "ippm"), ]

ggplot(
  res,
  aes(x = factor(measure, levels = c("ctfde", "ctfie", "ctfse", "tv")),
      y = value,
      fill = factor(outcome, levels = c("true", "northpointe")))
) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd),
                width=.2, position=position_dodge(.9)) +
  scale_fill_discrete(name = "Outcome",
                      labels = c(TeX("$Y^{true}$"), TeX("$\\hat{Y}^{NP}$"))) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.title =  element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    title = element_text(size = 16)
  ) +
  xlab("Fairness Measure") + ylab("Value") +
  scale_x_discrete(labels = c(TeX("Ctf-DE"),
                              TeX("Ctf-IE"),
                              TeX("Ctf-SE"),
                              TeX("TV"))) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle(TeX("TV$_{x_0,x_1}(y)$ and TV$_{x_0,x_1}(\\widehat{y})$ decompositions on COMPAS"))

ggsave("~/Desktop/compas-csp-cpp.png", width = 8, height = 5)
