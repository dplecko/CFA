
library(grf)
library(ggplot2)
library(faircause)
library(xgboost)
library(ranger)

data <- read.csv("rhc.csv")[,-1]

# construct the SFM
X <- "sex"
Z <- c("age", "race", "income", "edu", "ninsclas", "cat1")
D <- "swang1"
Y <- "dth30"
W <- setdiff(names(data), c(X, Z, Y, D, "sadmdte", "dthdte", "lstctdte",
                            "dschdte", "ptid", "death", "cat2", "adld3p", "urin1"))

# make the data entirely numeric
for (col in names(data)) {

  # move to 0, 1 if possible
  if (col == "sex") {
    data$sex <- as.integer(data$sex == "Male")
  } else if (col == "race") {
    data$race <- as.integer(data$sex == "Male")
  } else if (col == "ninsclas") {
    data$ninsclas <- factor(data$ninsclas,
                            levels = c("No insurance", "Medicare", "Medicaid",
                                       "Medicare & Medicaid", "Private",
                                       "Private & Medicare"))
    data$ninsclas <- as.integer(data$ninsclas)
  } else if (col == "cat1") {

    ohe <- as.data.frame(model.matrix(~0+data$cat1))
    names(ohe) <- unique(sort(data$cat1))

    names(ohe)[names(ohe) == "Colon Cancer"] <- "ColonCancer"
    names(ohe)[names(ohe) == "Lung Cancer"] <- "LungCancer"
    names(ohe)[names(ohe) == "MOSF w/Malignancy"] <- "MOSFMalig"
    names(ohe)[names(ohe) == "MOSF w/Sepsis"] <- "MOSFSepsis"

    data <- cbind(data, ohe)
    Z <- setdiff(c(Z, names(ohe)), "cat1")
    data$cat1 <- NULL
  } else if (col == "ca") {

    data$ca <- factor(data$ca, levels = c("No", "Yes", "Metastatic"))
    data$ca <- as.integer(data$ca)
  } else if (col == "income") {

    data$income <- factor(data$income, levels = c("Under $11k", "$11-$25k",
                                                  "$25-$50k", "> $50k"))
    data$income <- as.integer(data$income)
  } else if (all(data[[col]] %in% c("Yes", "No"))) {

    data[[col]] <- as.integer(data[[col]] == "Yes")
  } else if (col == "swang1") {

    data$swang1 <- as.integer(data$swang1 == "RHC")
  }
}

# apply causal forest
crf <- causal_forest(
  X = data[, c(X, Z, W)],
  W = data[[D]],
  Y = data[[Y]],
  tune.parameters = c("min.node.size", "mtry")
)

data$delta <- as.vector(crf$predictions)
cowplot::plot_grid(
  ggplot(data, aes(x = delta)) + geom_density() + theme_bw(),
  ggplot(data, aes(x = delta, color = factor(swang1))) + geom_density() +
    theme_bw()
)

# different estimation procedure? get the ATE using DR
# (can use faircause with X = D)
fcb <- fairness_cookbook(data[, c(X, Z, W, D, Y)],
                         X = D, Y = Y, Z = c(X, Z, W), W = NULL,
                         x0 = 0, x1 = 1, nboot1 = 5, nboot2 = 100,
                         tune_params = TRUE)

autoplot(fcb, decompose = "general")


# apply xgboost cross-validation
xgbcv <- xgb.cv(params = list(eta = 0.1),
                data = as.matrix(data[, c(X, Z, W, D)]), label = data[[Y]],
                nrounds = 100, early_stopping_rounds = 3, nfold = 10,
                metrics = "logloss")

# pick optimal number of rounds
nrounds <- xgbcv$best_iteration

# train the prediction object
xgb <- xgboost(params = list(eta = 0.1, objective = "binary:logistic"),
               data = as.matrix(data[, c(X, Z, W, D)]),
               label = data[[Y]], nrounds = nrounds)

# get the potential outcomes
data_d0 <- data_d1 <- data
data_d0[[D]] <- 0
data_d1[[D]] <- 1

py_d0 <- predict(xgb, as.matrix(data_d0[, c(X, Z, W, D)]))
py_d1 <- predict(xgb, as.matrix(data_d1[, c(X, Z, W, D)]))

plot(density(py_d1))

# try random forest
rf_lst <- lapply(
  c(5, 10, 20, 30, 50),
  function (mns) ranger(dth30 ~ ., data = data[, c(X, Z, W, D, Y)],
                        probability = TRUE, min.node.size = mns)
)

best.fit <- which.min(vapply(rf_lst, `[[`, "prediction.error",
                             FUN.VALUE = numeric(1L)))

rf <- rf_lst[[best.fit]]

py_d0 <- predict(rf, data_d0)$predictions[, 2]
py_d1 <- predict(rf, data_d1)$predictions[, 2]

plot(density(py_d1 - py_d0))

plot(density(sqrt(runif(1000))))
lines(density(1 - sqrt(runif(1000))), col = "red")
lines(density(1 - sqrt(1 - runif(1000))), col = "blue")
