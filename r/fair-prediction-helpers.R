
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

acc_measure <- function(y, p, loss = c("bce", "acc", "auc")) {

  loss <- match.arg(loss, c("bce", "acc", "auc"))

  if (loss == "bce") {
    p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
    ret <- -mean(y * log(p) + (1 - y) * log(1 - p))
  } else if (loss == "acc") {

    ret <- mean(round(p) == y)
  } else if (loss == "auc") {

    ret <- PRROC::roc.curve(scores.class0 = p, weights.class0 = y)$auc
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
        lmbd = lmbd)
}
