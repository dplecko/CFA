
preproc_data <- function(data, X, Z, W, Y) {

  SFM <- list(X = X, Z = Z, W = W, Y = Y)
  for (cmp in c("X", "Z", "W", "Y")) {

    for (var in SFM[[cmp]]) {

      if (is.numeric(data[[var]]) || is.integer(data[[var]])) next
      if (is.logical(data[[var]])) {

        data[[var]] <- as.integer(data[[var]])
        next
      }

      if (is.character(data[[var]])) data[[var]] <- as.factor(data[[var]])

      # only factors at this point
      if (length(levels(data[[var]])) == 2) {

        data[[var]] <- as.integer(data[[var]] == levels(data[[var]])[1])
      } else {

        enc_mat <- model.matrix(~ ., data = data.frame(var = data[[var]]))[, -1]
        colnames(enc_mat) <- paste0(var, seq_len(ncol(enc_mat)))

        # remove the factor column
        SFM[[cmp]] <- setdiff(SFM[[cmp]], var)
        data[[var]] <- NULL

        # add the encoded data
        SFM[[cmp]] <- c(SFM[[cmp]], colnames(enc_mat))
        data <- cbind(data, enc_mat)
      }
    }
  }

  list(data = data, sfm = SFM)
}

#' @importFrom xgboost xgb.DMatrix xgb.cv xgb.train
cv_xgb <- function(df, y, weights = NULL, ...) {

  dtrain <- xgb.DMatrix(data = as.matrix(df), label = y, weight = weights)

  binary <- all(y %in% c(0, 1))
  if (binary) {

    params <- list(objective = "binary:logistic", eval_metric = "logloss")
  } else {

    params <- list(objective = "reg:squarederror", eval_metric = "rmse")
  }

  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 1000,
    nfold = 5,
    early_stopping_rounds = 10,
    prediction = TRUE,
    verbose = FALSE, ...
  )

  xgb <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = cv$best_iteration,
    verbose = FALSE, ...
  )
  attr(xgb, "binary") <- binary

  xgb
}

pred_xgb <- function(xgb, df_test, intervention = NULL, X = "X") {

  if (!is.null(intervention)) {

    df_test[[X]] <- intervention
  }

  predict(xgb, as.matrix(df_test))
}

measure_spec <- function(spec = c("xspec", "ia")) {

  meas <- list()
  if (is.element("ia", spec)) {

    ia <- list(
      tese = list(
        sgn = c(1, -1, -1, 1),
        spc = list(
          c(0, 1, 1), c(0, 0, 0), c(1, 1, 1), c(1, 0, 0)
        ),
        ia = "TE x SE"
      ),
      deie0 = list(
        sgn = c(1, -1, -1, 1),
        spc = list(
          c(0, 0, 1), c(0, 0, 0), c(0, 1, 1), c(0, 1, 0)
        ),
        ia = "DE x IE"
      ),
      # deie1 = list(
      #   sgn = c(1, -1, -1, 1),
      #   spc = list(
      #     c(1, 0, 1), c(1, 0, 0), c(1, 1, 1), c(1, 1, 0)
      #   ),
      #   ia = "DE x IE"
      # ),
      dese = list(
        sgn = c(1, -1, -1, 1),
        spc = list(
          c(0, 0, 1), c(0, 0, 0), c(1, 0, 1), c(1, 0, 0)
        ),
        ia = "DE x SE"
      ),
      iese = list(
        sgn = c(1, -1, -1, 1),
        spc = list(
          c(0, 1, 0), c(0, 0, 0), c(1, 1, 0), c(1, 0, 0)
        ),
        ia = "IE x SE"
      ),
      deiese = list(
        sgn = c(c(1, -1, -1, 1), -c(1, -1, -1, 1)),
        spc = list(
          c(0, 0, 1), c(0, 0, 0), c(0, 1, 1), c(0, 1, 0),
          c(1, 0, 1), c(1, 0, 0), c(1, 1, 1), c(1, 1, 0)
        ),
        ia = "DE x IE x SE"
      )
    )
    meas <- c(meas, ia)
  }

  if (is.element("xspec", spec)) {

    xspec <- list(
      tv = list(
        sgn = c(1, -1),
        spc = list(c(1, 1, 1), c(0, 0, 0)),
        ia = "tv"
      ),
      ctfde = list(
        sgn = c(1, -1),
        spc = list(c(0, 0, 1), c(0, 0, 0)),
        ia = "ctfde"
      ),
      ctfie = list(
        sgn = c(1, -1),
        spc = list(c(0, 0, 1), c(0, 1, 1)),
        ia = "ctfie"
      ),
      ctfse = list(
        sgn = c(1, -1),
        spc = list(c(0, 1, 1), c(1, 1, 1)),
        ia = "ctfse"
      ),
      ett = list(
        sgn = c(1, -1),
        spc = list(c(0, 1, 1), c(0, 0, 0)),
        ia = "ett"
      )
    )
    meas <- c(meas, xspec)
  }

  meas
}

pso_logr <- function(cfit, data, X, Z, W, Y, ...) {

  n <- nrow(data)

  # un-nest the cfit object
  y_xzw <- cfit$y_xzw
  y_xz <- cfit$y_xz
  px_z <- cfit$px_z
  px_zw <- cfit$px_zw
  ey_nest <- cfit$ey_nest

  # get data and psos
  y <- data[[Y]]
  x <- data[[X]]
  pso <- list(list(list(list(), list()), list(list(), list())),
              list(list(list(), list()), list(list(), list())))

  for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1)) {

    pso[[xz+1]][[xw+1]][[xy+1]] <- rep(NA, n)
  }

  if (length(Z) == 0 & length(W) == 0) { # Case I: Z, W empty

    # get all the potential outcomes without cross-fitting
    for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1))
      pso[[xz+1]][[xw+1]][[xy+1]] <- log(mean(y[x==xy])) +
        (x == xy) / mean(x == xy) * (y / mean(y[x==xy]) - 1)
  } else if (length(Z) == 0) { # Z empty

    # get the total-effect pseudo-outcomes (ETT-like)
    for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1)) {

      # if (xz == xw & xz == xy) next
      pso[[xz+1]][[xw+1]][[xy+1]] <-
        (x == xy) / mean(x == xw) * px_zw[[xw+1]] / px_zw[[xw+1]] *
        (y - y_xzw[[xy+1]]) / y_xzw[[xy+1]] +
        (x == xw) / mean(x == xw) * log(y_xzw[[xy+1]])
    }
  } else if (length(W) == 0) { # Case III: W empty

    for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1)) {

      # if (xz == xw & xz == xy) next
      pso[[xz+1]][[xw+1]][[xy+1]] <-
        (x == xy) / mean(x == xz) * px_zw[[xz+1]] / px_zw[[xz+1]] *
        (y - y_xzw[[xy+1]]) / y_xzw[[xy+1]] +
        (x == xz) / mean(x == xz) * log(y_xzw[[xy+1]])
    }
  } else { # Case IV: Z, W non-empty

    for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1)) {

      # if (xy == xw & xy == xz) next
      pso[[xz+1]][[xw+1]][[xy+1]] <-
        # Term T1
        (x == xy) / mean(x == xz) *
        (y - y_xzw[[xy+1]]) / y_xzw[[xy+1]] *
        px_zw[[xw+1]] / px_zw[[xy+1]] *
        px_z[[xz+1]] / px_z[[xw+1]] +
        # Term T2
        (x == xw) / mean(x == xz) * px_z[[xz+1]] / px_z[[xw+1]] *
        (log(y_xzw[[xy+1]]) - ey_nest[[xy+1]]) +
        # Term T3
        (x == xz) / mean(x == xz) * ey_nest[[xy+1]]
    }
  }

  pso
}

pso_diff <- function(cfit, data, X, Z, W, Y, ...) {

  n <- nrow(data)

  # un-nest the cfit object
  y_xzw <- cfit$y_xzw
  y_xz <- cfit$y_xz
  px_z <- cfit$px_z
  px_zw <- cfit$px_zw
  ey_nest <- cfit$ey_nest

  # get data and psos
  y <- data[[Y]]
  x <- data[[X]]
  pso <- list(list(list(list(), list()), list(list(), list())),
              list(list(list(), list()), list(list(), list())))

  for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1)) {

    pso[[xz+1]][[xw+1]][[xy+1]] <- rep(NA, n)
  }

  # get E[Y | x] potential outcomes
  for (xz in c(0, 1))
    pso[[xz+1]][[xz+1]][[xz+1]] <- (x == xz) / mean(x == xz) * y

  if (length(Z) == 0 & length(W) == 0) { # Case I: Z, W empty

    # get all the potential outcomes without cross-fitting
    for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1))
      pso[[xz+1]][[xw+1]][[xy+1]] <- (x == xy) / mean(x == xy) * y

  } else if (length(Z) == 0) { # Z empty

    # get the total-effect pseudo-outcomes (ETT-like)
    for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1)) {

      if (xz == xw & xz == xy) next
      pso[[xz+1]][[xw+1]][[xy+1]] <-
        (x == xy) / mean(x == xw) * px_zw[[xw+1]] / px_zw[[xw+1]] *
        (y - y_xzw[[xy+1]]) +
        (x == xw) / mean(x == xw) * y_xzw[[xy+1]]
    }
  } else if (length(W) == 0) { # Case III: W empty

    for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1)) {

      if (xz == xw & xz == xy) next
      pso[[xz+1]][[xw+1]][[xy+1]] <-
        (x == xy) / mean(x == xz) * px_zw[[xz+1]] / px_zw[[xz+1]] *
        (y - y_xzw[[xy+1]]) +
        (x == xz) / mean(x == xz) * y_xzw[[xy+1]]
    }
  } else { # Case IV: Z, W non-empty

    for (xz in c(0, 1)) {

      xw <- xy <- 1 - xz
      pso[[xz+1]][[xw+1]][[xy+1]] <-
        # Term I
        (x == xw) / mean(x == xz) * px_z[[xz+1]] / px_z[[xw+1]] *
        (y - y_xz[[xw+1]]) +
        # Term II
        (x == xz) / mean(x == xz) * y_xz[[xw+1]]
    }

    for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1)) {

      if (xy == xw) next
      pso[[xz+1]][[xw+1]][[xy+1]] <-
        # Term T1
        (x == xy) * (y - y_xzw[[xy+1]]) * px_zw[[xw+1]] / px_zw[[xy+1]] *
        px_z[[xz+1]] / px_z[[xw+1]] * 1 / mean(x == xz) +
        # Term T2
        (x == xw) / mean(x == xz) * px_z[[xz+1]] / px_z[[xw+1]] *
        (y_xzw[[xy+1]] - ey_nest[[xy+1]]) +
        # Term T3
        (x == xz) / mean(x == xz) * ey_nest[[xy+1]]
    }
  }

  pso
}

cross_fit <- function(data, X, Z, W, Y, nested_mean, log_risk, ...) {

  if (length(Z) == 0 & length(W) == 0) {

    px <- rep(mean(data[[X]]), nrow(data))
    return(list(px_z = list(1-px, px), px_zw = list(1-px, px)))
  }

  # split into K folds
  n <- nrow(data)
  K <- 10
  folds <- sample(x = rep(1:K, each = ceiling(n / K)))[seq_len(n)]

  # elements to be filled
  y_xzw <- y_xz <- px_z <- px_zw <- ey_nest <- list(rep(NA, n), rep(NA, n))

  # x, y data
  y <- data[[Y]]
  x <- data[[X]]

  # cross-fit
  for (i in seq_len(K)) {

    # split into dev, val, tst
    tst <- folds == i
    dev <- folds %in% setdiff(seq_len(K), i)[1:6]
    val <- folds %in% setdiff(seq_len(K), i)[7:9]

    # develop models on dev
    if (length(Z) > 0) {

      mod_x_z <- cv_xgb(data[dev, Z], data[dev, X], ...)
      mod_y_xz <- cv_xgb(data[dev, c(X, Z)], data[dev, Y], ...)
    }

    if (length(W) > 0) {

      mod_x_zw <- cv_xgb(data[dev, c(Z, W)], data[dev, X], ...)
      mod_y_xzw <- cv_xgb(data[dev, c(X, Z, W)], data[dev, Y], ...)
    } else {

      # inherit from Z if W empty
      mod_x_zw <- mod_x_z
      mod_y_xzw <- mod_y_xz
    }

    # get the val set predictions (needed for nested means)
    px_zw_val <- pred_xgb(mod_x_zw, data[val, c(Z, W)])
    px_zw_val <- list(1 - px_zw_val, px_zw_val)

    if (length(Z) > 0) {

      px_z_val <- pred_xgb(mod_x_z, data[val, Z])
      px_z_val <- list(1 - px_z_val, px_z_val)
    }

    y_xzw_val <- list(
      pred_xgb(mod_y_xzw, data[val, c(X, Z, W)], intervention = 0, X = X),
      pred_xgb(mod_y_xzw, data[val, c(X, Z, W)], intervention = 1, X = X)
    )

    # get the test set values
    px_zw_tst <- pred_xgb(mod_x_zw, data[tst, c(Z, W)])
    px_zw[[1 + 0]][tst] <- 1 - px_zw_tst
    px_zw[[1 + 1]][tst] <- px_zw_tst

    if (length(Z) > 0) {

      px_z_tst <- pred_xgb(mod_x_z, data[tst, Z])
      px_z[[1 + 0]][tst] <- 1 - px_z_tst
      px_z[[1 + 1]][tst] <- px_z_tst
    } else {

      px_z[[1 + 0]][tst] <- 1 - mean(x)
      px_z[[1 + 1]][tst] <- mean(x)
    }

    y_xzw[[1 + 0]][tst] <- pred_xgb(mod_y_xzw, data[tst, c(X, Z, W)],
                                    intervention = 0, X = X)
    y_xzw[[1 + 1]][tst] <- pred_xgb(mod_y_xzw, data[tst, c(X, Z, W)],
                                    intervention = 1, X = X)

    if (length(Z) > 0) {

      y_xz[[1 + 0]][tst] <- pred_xgb(mod_y_xz, data[tst, c(X, Z)],
                                     intervention = 0, X = X)
      y_xz[[1 + 1]][tst] <- pred_xgb(mod_y_xz, data[tst, c(X, Z)],
                                     intervention = 1, X = X)
    }

    # nested means are not needed if either Z or W are empty
    if (length(Z) == 0 || length(W) == 0) next

    for (xw in c(0, 1)) {

      xy <- 1 - xw
      if (nested_mean == "wregr") {

        assertthat::assert_that(
          isFALSE(log_risk),
          msg = "Weighted regression not available for log-risk scale."
        )

        weights <- ifelse(
          x[val] == xy,
          px_z_val[[xy+1]] / px_z_val[[xw+1]] *
            px_zw_val[[xw+1]] / px_zw_val[[xy+1]],
          px_z_val[[xy+1]] / px_z_val[[xw+1]]
        )

        mod_nested <- cv_xgb(data[val, Z], data[val, Y], weights = weights,
                             ...)
        ey_nest[[xy+1]][tst] <- pred_xgb(mod_nested, data[val, Z])
      } else if (nested_mean == "refit") {

        y_tilde <- pred_xgb(mod_y_xzw, data[val, c(X, Z, W)],
                            intervention = xy, X = X)
        if (log_risk) y_tilde <- log(y_tilde)
        mod_nested <- cv_xgb(data[val, c(X, Z)], y_tilde, ...)
        ey_nest[[xy+1]][tst] <- pred_xgb(mod_nested, data[tst, c(X, Z)],
                                         intervention = xw, X = X)
      }
    }
  }

  list(
    y_xzw = y_xzw,
    y_xz = y_xz,
    px_z = px_z,
    px_zw = px_zw,
    ey_nest = ey_nest
  )
}

one_step_debias <- function(data, X, Z, W, Y, nested_mean = c("refit", "wregr"),
                            log_risk = FALSE, eps_trim = 0, ...) {

  nested_mean <- match.arg(nested_mean, c("refit", "wregr"))
  cfit <- cross_fit(data, X, Z, W, Y, nested_mean, log_risk, ...)

  if (log_risk) {
    pso <- pso_logr(cfit, data, X, Z, W, Y, ...)
  } else {
    pso <- pso_diff(cfit, data, X, Z, W, Y, ...)
  }

  # get extreme propensity weights
  extrm_pxz <- (cfit$px_z[[1]] < eps_trim) | (1 - cfit$px_z[[1]] < eps_trim)
  extrm_pxzw <-  (cfit$px_zw[[1]] < eps_trim) | (1 - cfit$px_zw[[1]] < eps_trim)
  extrm_idx <- extrm_pxz | extrm_pxzw

  # report if large number of propensity weights below specified threshold
  if (mean(extrm_idx) > 0.02) {
    message(round(100 * mean(extrm_idx), 2),
            "% of extreme P(x | z) or P(x | z, w) probabilities at threshold",
            " = ", eps_trim, ".\n",
            "Reported results are for the overlap population. ",
            "Consider investigating overlap issues.")
  }

  # trim population to extreme weights
  for (xz in c(0, 1)) for (xw in c(0, 1)) for (xy in c(0, 1))
    pso[[xz+1]][[xw+1]][[xy+1]][extrm_idx] <- NA

  # get specification of measures to be reported
  ias <- measure_spec()

  res <- c()
  scale <- if (isFALSE(log_risk)) "difference" else "log-risk"
  for (i in seq_along(ias)) {

    pseudo_out <- 0
    for (j in seq_along(ias[[i]]$sgn)) {

      xz <- ias[[i]]$spc[[j]][1]
      xw <- ias[[i]]$spc[[j]][2]
      xy <- ias[[i]]$spc[[j]][3]
      pseudo_out <- pseudo_out + ias[[i]]$sgn[j] * pso[[xz+1]][[xw+1]][[xy+1]]
    }
    psi_osd <- mean(pseudo_out, na.rm = TRUE)
    dev <- sqrt(var(pseudo_out, na.rm = TRUE) / sum(!is.na(pseudo_out)))

    res <- rbind(
      res,
      data.frame(measure = ias[[i]]$ia, value = psi_osd, sd = dev, scale = scale)
    )
  }

  pw <- list(px_z = cfit$px_z, px_zw = cfit$px_zw)
  attr(res, "pw") <- pw
  res
}
