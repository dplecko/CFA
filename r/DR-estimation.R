

regr <- function(y, x) {
  
  #'* Allow for other classifiers apart from ranger *
  
  if (setequal(unique(y), c(0, 1))) {
    # probability case
    return(ranger(y = y, x = x, probability = TRUE))
  } else {
    # regression case
    return(ranger(y = y, x = x))
  }
  
}

pred <- function(mod, x) {
  
  if (mod$treetype == "Probability estimation") {
    return(predict(mod, x)$predictions[, 2])
  } else {
    return(predict(mod, x)$predictions)
  }
  
}

model_based <- function() {
  return(NULL)
}

doubly_robust <- function(x, z, w, y, K = 5) {
  
  folds <- as.integer(cut(seq_along(x), breaks = 10))
  y0 <- y1 <- y0w1 <- y1w0 <- rep(NA_real_, seq_along(x))
  
  for (k in seq_len(K)) {
    
    #'* Improve the notation *
    
    #'* Subsetting of form z[part, ] is tricky when Z has a single column *
    
    part <- which(folds == k)
    comp <- which(folds != k)
    
    # total effects
    
    # regress X on Z
    x_z_mod <- regr(x[comp], z[comp, ])
    
    # regress Y on X + Z
    y_xz_mod <- regr(y[comp], cbind(x[comp], z[comp, ]))
    
    # make predictions on the target partition
    x_z_pred <- pred(x_z_mod, z[part, ])
    
    
    #'* Need to check here -- regress on Z only within X = 0, or on X+Z ? *
    
    y0_xz_pred <- pred(y_xz_mod, cbind(x = 0, z[part, ]))
    y1_xz_pred <- pred(y_xz_mod, cbind(x = 1, z[part, ]))
    
    y0[part] <- (y[part] - y0_xz_pred) * (x[part] == 0) / (1 - x_z_pred) +
                y0_xz_pred
    y1[part] <- (y[part] - y1_xz_pred) * (x[part] == 1) / (x_z_pred) +
                y1_xz_pred
    
    # nested effects
    
    # regress X on Z + W
    x_zw_mod <- regr(x[comp], cbind(z, w)[comp, ])
    x_zw_pred <- pred(x_zw_mod, cbind(z, w)[part, ])
    
    # split complement into two equal parts
    
    # part 1: learn the mean
    mupr <- sample(comp, length(comp) / 2)
    nest <- setdiff(comp, mupr) 
    # regress Y on X + Z + W
    y_xzw_mod <- regr(y[mupr], )
    
    # part 2: learn nested mean
    
    #'* Need to check here -- regress on Z+W only within X = 0, or on X+Z+W? *
    y0_xzw_pred <- pred(y_xzw_mod, cbind(x = 0, z[nest,], w[nest, ]))
    ey0_xzw_mod <- regr(y0_xzw_pred, cbind(x = 0, z[nest,], w[nest, ]))
    
    # part 3: compute the mean / nested mean on target partition
    y0_xzw_predte <- pred(ey0_xzw_mod, cbind(x = 0, z[part,], w[part, ]))
    ey0_xzw_pred <- pred(ey0_xzw_mod, cbind(x = 0, z[part,], w[part, ]))
    
    y0w1[part] <- 
      (x_zw_pred) * (x[part]==0) / ((1 - x_zw_pred) * x_z_pred) * 
        (y[part] - y0_xzw_predte) +
      (x[part]==1) / (1 - x_z_pred) * (y0_xzw_predte - ey0_xzw_pred) +
      ey0_xzw_pred
    
  }
  
  #'* Could add trimming based on extreme probabilities *
  return(list(y0, y1, y0w1))
  
} 


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
