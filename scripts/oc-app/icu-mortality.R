
Sys.setenv("RICU_CONFIG_PATH" = "~/fairness/CFA/scripts/oc-app")
Sys.setenv("RICU_SRC_LOAD" =
             "mimic,miiv,aumc,hirid,eicu,eicu_demo,mimic_demo,anzics,sic")

library(ricu)
library(ggplot2)
library(icd)
library(data.table)
library(faircause)
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
r_dir <- file.path(root, "scripts", "oc-app", "helpers")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))


dat <- load_concepts(c("death", "ethnicity", "adm_episode", "bmi"), "miiv")
dat <- dat[adm_episode == 1 & !is.na(bmi)]
dat <- dat[ethnicity == "Hispanic", ethnicity := "African American"]
dat[, mean(is_true(death)), by = "ethnicity"]


# perform a causal decomposition
patient_ids <- id_col(load_concepts("adm_episode", "miiv")[adm_episode == 1])
dat <- load_concepts(c("acu_24", "diag", "age", "sex", "charlson",
                       "lact_24", "pafi_24", "ast_24",
                       "ethnicity", "death"), "miiv", patient_ids = patient_ids)
dat <- dat[ethnicity %in% c("Caucasian", "African American")]
dat[, c(index_var(dat)) := NULL]
imp_lst <- list(
  age = 65,
  acu_24 = 0,
  charlson = 0,
  lact_24 = 1,
  ast_24 = 20,
  pafi_24 = 500,
  death = FALSE
)

for (i in seq_len(ncol(dat))) {

  var <- names(dat)[i]
  if (any(is.na(dat[[var]])) & !is.null(imp_lst[[var]]))
    dat[is.na(get(var)), c(var) := imp_lst[[var]]]
}

fcb <- fairness_cookbook(
  data = dat, X = "ethnicity", Z = c("age", "sex"),
  W = c("acu_24", "diag", "charlson"), Y = "death",
  x1 = "Caucasian", x0 = "African American", nboot2 = 5
)
autoplot(fcb) +
  labs(title="Causal decomposition of mortality difference: MIMIC-IV race effect",
       y="Mortality difference (%)") +
  theme_minimal() +
  scale_x_discrete(labels = c("Total Variation", "Direct", "Indirect",
                                "Confounded"), name = "Pathway") +
  scale_fill_discrete(labels = c("Total Variation", "Direct", "Indirect",
                                 "Confounded"), name = "Pathway")

ggsave("~/Desktop/mortality.png", width = 7, height = 4, bg = "white",
       dpi = 500)


# investigate the age data
age_dat <- load_concepts(c("age", "ethnicity"), "miiv")
ggplot(age_dat[ethnicity %in% c("Caucasian", "African American")],
       aes(x = age, fill = ethnicity)) +
  geom_density()

# very rough analysis
oxy <- load_concepts(c("safi_24", "pafi_24", "death"), c("miiv", "aumc", "sic"))

# input death
oxy[is.na(death), death := FALSE]
oxy[, c(index_var(oxy)) := NULL]
oxy <- oxy[complete.cases(oxy)]

# get the AUC for mortality
library(pROC)
library(data.table)
library(ggplot2)

# Assuming oxy is your data.table
sources <- unique(oxy$source)
auc_data <- data.frame(Source = character(), Variable = character(), AUC = numeric(), stringsAsFactors = FALSE)

for (src in sources) {
  subset_data <- oxy[source == src]

  # pafi_24
  roc_pafi <- roc(subset_data$death, subset_data$pafi_24)
  auc_data <- rbind(auc_data, data.frame(Source = src, Variable = "PAFI", AUC = roc_pafi$auc))

  # safi_24
  roc_safi <- roc(subset_data$death, subset_data$safi_24)
  auc_data <- rbind(auc_data, data.frame(Source = src, Variable = "SAFI", AUC = roc_safi$auc))
}

# Plotting
ggplot(auc_data, aes(x = Source, y = AUC, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "AUC by Source for PAFI and SAFI", y = "AUC Value", x = "Source") +
  geom_text(aes(label = round(AUC, 3)), position = position_dodge(width = 0.9), vjust = -0.25)

