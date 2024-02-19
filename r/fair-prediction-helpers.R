
verify_numeric_input <- function(data) {

  dtypes <- vapply(data, function(x) is.numeric(x) | is.integer(x), logical(1L))
  if (!all(dtypes)) {

    stop(
      paste(
        "Only `numeric` and `integer` values currently supported as inputs",
        "for `fair_prediction()` and `fair_decision()` functionality.\n",
        "If you have `factor`s or `logical`s, please convert them using",
        "one-hot encoding."
      )
    )
  }
}


compute_auc <- function(out, pred) {

  if(!all(out %in% c(0, 1))) return(0)
  if(any(pred < 0 | pred > 1)) return(0)

  # Combine and sort by predicted probabilities in descending order
  data <- data.frame(out, pred)
  data <- data[order(data$pred, decreasing = TRUE),]

  # Calculate TPR and FPR
  n_pos <- sum(data$out == 1)
  n_neg <- sum(data$out == 0)
  cum_pos_rate <- cumsum(data$out == 1) / n_pos
  cum_neg_rate <- (1:nrow(data) - cumsum(data$out == 1)) / n_neg

  # Calculate AUC using trapezoidal rule
  auc <- sum((cum_neg_rate[-1] - cum_neg_rate[-nrow(data)]) *
               (cum_pos_rate[-1] + cum_pos_rate[-nrow(data)]) / 2)

  return(auc)
}

acc_measure <- function(y, p, loss = c("bce", "acc", "auc", "mse")) {

  loss <- match.arg(loss, c("bce", "acc", "auc", "mse"))

  if (loss == "bce") {
    p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
    ret <- -mean(y * log(p) + (1 - y) * log(1 - p))
  } else if (loss == "acc") {

    ret <- mean(round(p) == y)
  } else if (loss == "auc") {

    ret <- compute_auc(y, p)
  } else if (loss == "mse") {

    ret <- mean((y - p)^2)
  }

  ret
}

acc_measure_boot <- function(y, p, loss) {

  sd(vapply(
    1:100,
    function(i) {

      boot_idx <- sample.int(length(y), replace = TRUE)
      acc_measure(y[boot_idx], p[boot_idx], loss)
    }, numeric(1L)
  ))
}

lambda_performance <- function(meas, y, p, lmbd) {

  cbind(meas[meas$measure %in% c("nde", "nie", "expse_x1", "expse_x0"), ],
        bce = acc_measure(y, p, "bce"),
        bce_sd = acc_measure_boot(y, p, "bce"),
        acc = acc_measure(y, p, "acc"),
        acc_sd = acc_measure_boot(y, p, "acc"),
        auc = acc_measure(y, p, "auc"),
        auc_sd = acc_measure_boot(y, p, "auc"),
        mse = acc_measure(y, p, "mse"),
        mse_sd = acc_measure_boot(y, p, "mse"),
        lmbd = lmbd)
}
