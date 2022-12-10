

DoubleRobustCausalExpTV <- function(data, X, W, Z, Y, x0, x1, ...) {
  
  idx <- data[[X]] == x0
  int.data <- data
  int.data[[X]] <- factor(x1, levels = levels(data[[X]]))
  
  # get TV
  tv <- mean(data[[Y]][!idx]) - mean(data[[Y]][idx])
  
  # get DE
  cond_zw <- paste(paste(W, collapse = " + "), "+", paste(Z, collapse = " + "))
  
  de <- DR_proced(data, int.data, Y, X, cond_zw, idx, ...) -
    mean(data[[Y]][idx])
  
  # get SE
  cond_z <- paste(Z, collapse = " + ")
  
  pyx1_x0 <- DR_proced(data, int.data, Y, X, cond_z, idx, ...)
  
  se <- pyx1_x0 - mean(data[[Y]][!idx])
  
  # get ETT
  ett <- pyx1_x0 - mean(data[[Y]][idx])
  
  cef <- list(TV = tv, DE = de, SE = se, ETT = ett, IE = de - ett)
  
}

c_eff <- function(form, data, int.data, ...) {
  
  rf <- ranger::ranger(form, data = data, keep.inbag = T,
                       importance = "impurity", ...)
  assertthat::assert_that(rf$treetype %in% c("Regression",
                                             "Probability estimation"))
  
  if (rf$treetype == "Probability estimation") {
    
    p2 <- predict(rf, int.data, predict.all = T)$predictions[, 2, ]
    
  } else {
    
    p2 <- predict(rf, int.data, predict.all = T)$predictions
    
  }
  
  oob.matrix <- Reduce(cbind, lapply(rf$inbag.counts, function(x=i) x == 0))
  
  p2 <- rowSums(p2 * oob.matrix) / rowSums(oob.matrix)
  
  p2
  
}

DR_estimate <- function(y, y_model, prop, idx) {
  
  # prop is propensity for idx
  sum((y - y_model) * prop / (1 - prop) * !idx) / sum(idx) + mean(y_model[idx])
  
}

DR_proced <- function(data, int.data, Y, X, cond, idx, ...) {
  
  form_mean <- as.formula(paste(Y, "~", X, "+", cond))
  
  form_prop <- as.formula(paste(X, "~", cond))
  
  y_model <- model_mean(form_mean, data, int.data, ...)
  x_prop <- model_propensity(form_prop, data, unique(data[[X]][idx]), ...)
  
  eps <- 0.001
  x_prop[x_prop == 0] <- eps
  x_prop[x_prop == 1] <- 1 - eps
  
  DR_estimate(data[[Y]], y_model, x_prop, idx)
  
}
