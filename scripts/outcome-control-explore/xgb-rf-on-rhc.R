
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
