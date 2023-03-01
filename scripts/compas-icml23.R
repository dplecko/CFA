root <- rprojroot::find_root(rprojroot::is_git_root)

library(fairadapt)
library(faircause)
library(ranger)
library(ggplot2)
library(latex2exp)

# helpers
compute_ippm <- function(pred, out, x, x0, name, nboot = 100) {

  breaks <- quantile(pred, probs = seq(0.1, 0.9, 0.1))
  bins <- .bincode(pred, c(-Inf, breaks, Inf))

  idx <- x == x0

  bin_diffs <- tapply(out[!idx], bins[!idx], mean) -
    tapply(out[idx], bins[idx], mean)

  ippm <- vapply(
    seq_len(nboot),
    function(i) {
      boot <- sample.int(length(pred), replace = TRUE)

      out_b <- out[boot]
      idx_b <- idx[boot]
      bins_b <- bins[boot]

      bin_diffs <- tapply(out_b[!idx_b], bins_b[!idx_b], mean) -
        tapply(out_b[idx_b], bins_b[idx_b], mean)

      mean(bin_diffs)
    }, numeric(1L)
  )

  data.frame(value = mean(ippm), sd = sd(ippm), measure = "ippm",
             outcome = name)
}

set.seed(23)
data <- read.csv(file.path(root, "compas-scores-two-years.csv"))
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
np_ippm <- compute_ippm(data$decile_score / 10, data$two_year_recid,
                         x = data$race, x0 = "Majority", "northpointe")
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

# obtain fair predictions

# get adjacency matrix
vars <- c("age", "sex", "juv_fel","juv_misd",
          "juv_other", "priors",
          "charge", "race", "two_year_recid")

adj.mat <- array(0, dim = c(length(vars), length(vars)))
colnames(adj.mat) <- vars
rownames(adj.mat) <- colnames(adj.mat)

# adding the edges to the matrix
adj.mat[
  c("race", "sex", "age"), c("juv_fel", "juv_misd",
                             "juv_other", "priors",
                             "charge", "two_year_recid")] <- 1
adj.mat[c("juv_fel", "juv_misd", "juv_other"),
        c("priors", "charge", "two_year_recid")] <- 1
adj.mat["priors", c("charge", "two_year_recid")] <- 1
adj.mat["charge", "two_year_recid"] <- 1

cfd.mat <- adj.mat
cfd.mat[, ] <- 0L
cfd.mat[c("sex", "age"), "race"] <- cfd.mat["race", c("sex", "age")] <- 1L

fdp <- fairadapt::fairadapt(two_year_recid ~ ., prot.attr = "race",
                            train.data = data[, vars], adj.mat = adj.mat)

# obtain the adapted data
ad_dat <- fairadapt:::adaptedData(fdp)
ad_dat$race <- data$race

# obtain predictions based on the adapted data
adapt_rf <- ranger(two_year_recid ~ ., ad_dat,
                   probability = TRUE)
adapt_oob_prob <- adapt_rf$predictions[, 2]
fp_ippm <- compute_ippm(adapt_oob_prob, data$two_year_recid,
                        data$race, "Majority", "fairadapt")

# match the prevalence with NP and true
adapt_rf_class <- ranger(two_year_recid ~ ., ad_dat,
                         classification = TRUE)
adapt_oob <- adapt_rf_class$predictions

# recover probabilities too
ad_dat$two_year_recid <- adapt_oob

# decomposing the fair predictor
fairadapt_tvd <- fairness_cookbook(
  ad_dat, X = X, W = W, Z = Z, Y = "two_year_recid",
  x0 = "Majority", x1 = "Minority", nboot1 = 5
)

# side-by-side plot of three decompositions
res <- rbind(
  cbind(summary(two_year)$measures, outcome = "true"),
  cbind(summary(northpointe)$measures, outcome = "northpointe"),
  cbind(summary(fairadapt_tvd)$measures, outcome = "fairadapt"),
  np_ippm,
  fp_ippm
)


res <- res[res$measure %in% c("ctfde", "ctfie", "ctfse", "tv", "ippm"), ]

ggplot(
  res,
  aes(x = factor(measure, levels = c("ctfde", "ctfie", "ctfse", "tv", "ippm")),
      y = value,
      fill = factor(outcome, levels = c("true", "northpointe", "fairadapt")))
  ) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd),
                width=.2, position=position_dodge(.9)) +
  scale_fill_discrete(name = "Outcome",
                      labels = c(TeX("$Y^{true}$"), TeX("$\\hat{Y}^{NP}$"),
                                 TeX("$\\hat{Y}^{FP}$"))) +
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
                              TeX("TV"),
                              TeX("iPPM"))) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Fairness Measures on the COMPAS dataset")
