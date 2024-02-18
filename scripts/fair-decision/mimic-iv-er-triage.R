
# MIMIC-IV: Y = in-hospital mortality, D = ICU admission
# mean(!is.na(miiv$admissions$deathtime))
# 
# hosp <- load_id("admissions", "miiv")
# icu <- load_id("icustays", "miiv")
# 
# hosp[, death := !is.na(deathtime)]
# 
# hosp[, icu_adm := subject_id %in% icu$stay_id]
# 
# table(hosp$death, hosp$icu_adm)
# 
# 
# # MIMIC-IV ED prep
# 
# csv_to_fst <- function(tbl) {
#   
#   res <- read.csv(paste0("~/Desktop/mimic-iv-ed-2.2/ed/", tbl, ".csv"))
#   fst::write.fst(res, path = file.path(data_dir(), "miiv", paste0(tbl, ".fst")))
# }
# 
# csv_to_fst("edstays")
# csv_to_fst("triage")
# 
# tbl <- "triage"
# dat <- read.csv(paste0("~/Desktop/mimic-iv-ed-2.2/ed/", tbl, ".csv"))

library(ricu)
library(ggplot2)
library(grf)
library(xgboost)
library(ranger)
library(faircause)

# cohort: those who are admitted
ed <- load_id("edstays", "miiv")
ed <- ed[disposition == "ADMITTED"]
ed <- ed[, c("stay_id", "subject_id", "hadm_id", "gender", "race")]

# decision: acuity
trg <- load_id("triage", "miiv")

ed <- merge(ed, trg, by = c("stay_id", "subject_id"))

# add mortality from MIMIC-IV general
adm <- data.table::as.data.table(miiv$admissions)
adm <- adm[, c("hadm_id", "deathtime")]

ed <- merge(ed, adm, by = "hadm_id")
ed[, death := as.integer(!is.na(deathtime))]

ed[, race_code := ifelse(race == "WHITE", 1, 0)]
ed[, decision := as.integer(acuity <= 2)]
ed[, gender_code := ifelse(gender == "M", 1, 0)]

# fix the pain column
ed[grepl("critical|uta|ua|unable", pain, ignore.case = TRUE), pain_code := 10]
ed[ed$pain %in% 0:10, pain_code := as.integer(pain)]

# check mortality by acuity x {race, gender}
ggplot(
  ed[, list(mortality = mean(death)), by = c("acuity", "gender")],
  aes(x = acuity, y = mortality, color = factor(gender))
) + geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom")

ggplot(
  ed[, list(mortality = mean(death)), by = c("acuity", "race_code")],
  aes(x = acuity, y = mortality, color = factor(race_code))
) + geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom")

# create the SFM
X <- "gender_code"
Z <- "race_code"
W <- c("temperature", "heartrate", "resprate", "o2sat", "sbp", "dbp", 
       "pain_code")
D <- "decision"
Y <- "death"

# select the cohort
coh_idx <- complete.cases(ed[, c(X, Z, W, D, Y), with = FALSE])

ed_fin <- ed[coh_idx]

crf <- causal_forest(
  X = as.matrix(ed_fin[, c(X, Z, W), with = FALSE]), 
  W = ed_fin[[D]], 
  Y = ed_fin[[Y]],
  tune.parameters = c("min.node.size", "mtry")
)

# check if any effect of D (causal forest / xgboost / rf)
ed_fin$delta <- as.vector(crf$predictions)
ggplot(ed_fin, aes(x = delta)) + geom_density() + theme_bw()

# look at allocation of treatment according to benefit (!)