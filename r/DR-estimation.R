
model_mean <- function(form, data, int.data, ...) {

  rf <- ranger(form, data = data, keep.inbag = T, importance = "impurity", ...)
  assertthat::assert_that(rf$treetype %in% c("Regression",
                                             "Probability estimation"))

  if (rf$treetype == "Probability estimation") {

    p2 <- predict(rf, int.data, predict.all = T)$predictions[, 2, ]

  } else {

    p2 <- predict(rf, int.data, predict.all = T)$predictions

  }

  oob.matrix <- Reduce(cbind, lapply(rf$inbag.counts, function(x=i) x == 0))

  rowSums(p2 * oob.matrix) / rowSums(oob.matrix)

}

model_propensity <- function(form, data, xlvl, ...) {

  assertthat::assert_that(length(xlvl) == 1L)

  rf <- ranger(form, data = data, keep.inbag = TRUE, importance = "impurity", 
               probability = TRUE, ...)
  assertthat::assert_that(rf$treetype == "Probability estimation")

  rf$predictions[, xlvl]

}

DR_estimate <- function(y, y_model, prop, idx) {

  # prop is propensity for idx
  sum((y - y_model) * prop / (1 - prop) * !idx) / sum(idx) + mean(y_model[idx])

}

DR_proced <- function(data, int.data, Y, X, cond, idx, ...) {

  form_mean <- as.formula(paste(Y, "~", X, "+", cond))

  form_prop <- as.formula(paste(X, "~", cond))

  y_model <- model_mean(form_mean, data, int.data, ...)
  x_prop <- model_propensity(form_prop, data, unique(data[[X]][idx]))
  
  eps <- 0.001
  x_prop[x_prop == 0] <- eps
  x_prop[x_prop == 1] <- 1 - eps

  DR_estimate(data[[Y]], y_model, x_prop, idx)

}
