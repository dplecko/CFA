
library(faircause)
library(ricu)
library(ggplot2)

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

data <- load_resp_data("mimic_demo")
data <- as.data.frame(data)

# Standard Fairness Model
X <- "sex"
Z <- "age"
W <- c("sofa", "po2", "o2prior")
D <- "respirator"
Y <- "o2post"

# fit the outcome control model
resp_oc <- fair_decisions(
  data, X = X, Z = Z, W = W, Y = Y, D = D, x0 = 0, x1 = 1,
  delta_transform = function(x) ifelse(x < 97, -(x-97)^2, 0)
)

# inspect the decomposition of D
autoplot(resp_oc, type = "decision")

# inspect the decomposition of \Delta
autoplot(resp_oc, type = "delta")

# inspect benefit fairness
autoplot(resp_oc, type = "benefit_fairness")

# make predictions on test
test_data <- data
fair_dec <- predict(resp_oc, newdata = test_data, budget = 0.2)

fair_dec$delta
fair_dec$decision
