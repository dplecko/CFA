
Sys.setenv("RICU_CONFIG_PATH" = "~/fairness/CFA/scripts/oc-app")
Sys.setenv("RICU_SRC_LOAD" =
             "mimic,miiv,aumc,hirid,eicu,eicu_demo,mimic_demo,anzics,sic")

library(ricu)
library(ggplot2)
library(ranger)
library(data.table)
library(faircause)
library(xgboost)
library(latex2exp)

r_dir <- file.path("~/ICU/op-and-sex", "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

load_resp_data <- function(src) {

  dat <- load_concepts(
    c("vent_ind", "resp", "po2", "sofa", "o2sat", "sex", "age"), src
  )

  dat <- dat[get(index_var(dat)) <= hours(48L) &
               get(index_var(dat)) >= hours(0L)]
  dat[is.na(vent_ind), vent_ind := FALSE]

  dat[, is_vent := cummax(vent_ind), by = c(id_vars(dat))]

  cand <- unique(id_col(dat[o2sat <= 96 & is_vent == 0]))
  cdat <- dat[id_col(dat) %in% cand]
  cdat <- replace_na(cdat, type = "locf", vars = c("o2sat", "po2", "resp"))
  cdat[is.na(po2), po2 := median(po2, na.rm = TRUE)]
  cdat[is.na(resp), resp := median(resp, na.rm = TRUE)]

  # lag by 3 hours both ways
  cdat[, is_vent_lag3 := data.table::shift(is_vent, -3L)]
  cdat[, is_vent_lagrev3 := data.table::shift(is_vent, 3L)]

  # the actioned cohort
  act <- merge(
    cdat[is_vent == 0 & is_vent_lag3 == 1,
         list(o2prior = mean(o2sat, na.rm = TRUE), sofa = max(sofa),
              resp = mean(resp, na.rm = TRUE), po2 = mean(po2, na.rm = TRUE),
              sex = unique(sex), age = unique(age)),
         by = c(id_vars(dat))],
    cdat[is_vent == 1 & is_vent_lagrev3 == 0,
         list(o2post = mean(o2sat, na.rm = TRUE)),
         by = c(id_vars(dat))]
  )

  act[, respirator := 1]

  # take complete cases
  act <- act[complete.cases(act)]

  # the non-actioned cohort
  ctrls <- id_col(cdat[, max(is_vent), by = c(id_vars(cdat))][V1 == 0])
  ndat <- cdat[(id_col(cdat) %in% ctrls)]

  skp <- merge(
    ndat[get(index_var(ndat)) %in% hours(10, 11, 12),
         list(o2prior = mean(o2sat, na.rm = TRUE), sofa = max(sofa),
              resp = mean(resp, na.rm = TRUE), po2 = mean(po2, na.rm = TRUE),
              sex = unique(sex), age = unique(age)),
         by = c(id_vars(dat))],
    ndat[get(index_var(ndat)) %in% hours(13, 14, 15),
         list(o2post = mean(o2sat, na.rm = TRUE)),
         by = c(id_vars(dat))]
  )
  skp <- skp[, respirator  := 0]
  skp <- skp[complete.cases(skp)]

  res <- rbind(act, skp)
  res[, sex := ifelse(sex == "Male", 1, 0)]
  res
}

est_delta <- function(res) {

  X <- "sex"
  Z <- "age"
  W <- c("sofa", "po2", "resp", "o2prior")
  D <- "respirator"
  Y <- "o2post"

  xgbcv <- xgb.cv(params = list(eta = 0.1),
                  data = as.matrix(res[, c(X, Z, W, D), with = FALSE]),
                  label = res[[Y]], nrounds = 100, early_stopping_rounds = 3,
                  nfold = 10)

  # pick optimal number of rounds
  nrounds <- xgbcv$best_iteration

  # train the prediction object
  xgb <- xgboost(params = list(eta = 0.1),
                 data = as.matrix(res[, c(X, Z, W, D), with = FALSE]),
                 label = res[[Y]], nrounds = nrounds)

  res0 <- res1 <- copy(res)
  res0[[D]] <- 0
  res1[[D]] <- 1

  y0 <- predict(xgb, as.matrix(res0[, c(X, Z, W, D), with = FALSE]))
  y1 <- predict(xgb, as.matrix(res1[, c(X, Z, W, D), with = FALSE]))

  y1[y1 < y0] <- y0[y1 < y0]
  o2sat_loss <- function(x) ifelse(x < 97, -(x-97)^2, 0)

  o2sat_loss(y1) - o2sat_loss(y0)
}

plt_save <- function(src) {

  src_lbl <- function(x) {
    if (x == "miiv") return("MIMIC-IV") else return("AmsterdamUDB")
  }

  res <- load_resp_data(src)
  delta <- est_delta(res)
  res[, benefit := delta]

  # break the zeros at random for convenience
  res[benefit == 0, benefit := runif(seq_along(benefit), max = 10^(-15))]

  # group into deciles
  res[, dec := .bincode(benefit, quantile(benefit, seq(0, 1, 0.1)),
                        include.lowest = TRUE)]

  # plot
  p <- ggplot(res[, list(V1 = mean(respirator), V2 = .N), by = c("sex", "dec")],
         aes(x = dec, y = V1, color = factor(sex))) +
    geom_line() + geom_point() + theme_bw() +
    geom_ribbon(
      aes(ymin = V1 - sqrt(1.96 * V1 * (1-V1) / V2),
          ymax = V1 + sqrt(1.96 * V1 * (1-V1) / V2),
          fill = factor(sex)),
      linewidth = 0, alpha = 0.2
    ) +
    scale_x_continuous(breaks = 1:10, labels = paste0("D", 1:10)) +
    ylab("P(mechanical ventilation)") + xlab("Benefit Decile") +
    scale_color_discrete(name = "Sex", labels = c("Female", "Male")) +
    scale_fill_discrete(name = "Sex", labels = c("Female", "Male")) +
    ggtitle(paste("Benefit Fairness on", src_lbl(src))) +
    theme(legend.position = c(0.2, 0.7), legend.box.background = element_rect())

  ggsave(plot = p, filename = paste0("BF_", src, ".png"),
         width = 6, height = 4.1)
}

# for (src in c("miiv", "mimic", "aumc", "hirid", "sic")) plt_save(src)
set.seed(2024)
plt_save("miiv")
plt_save("aumc")
