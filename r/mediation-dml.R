
tune_params_ranger <- function(y, x, type = c("prob", "regr"), ...) {

  mns_grid <- switch(type, prob = c(10, 20, 50, 100), regr = c(5, 10, 25, 50))
  prob <- switch(type, prob = TRUE, regr = FALSE)

  mods <- lapply(
    mns_grid,
    function(mns) {

      mod <- ranger::ranger(y = y, x = x, probability = prob,
                            min.node.size = mns, ...)
    }
  )

  oob_err <- vapply(mods, function(mod) mod$prediction.error, numeric(1L))
  mods[[which.min(oob_err)]]
}

regr <- function(y, x, model = "ranger", tune_params = FALSE, mns = NULL, ...) {

  # Z set empty -> x equals NULL -> return just the mean
  if (is.null(unlist(x))) return(mean(y))

  if (setequal(unique(y), c(0, 1))) {

    # probability case
    if (model == "ranger") {

      if (tune_params & is.null(mns)) {

        mod <- tune_params_ranger(y, x, "prob")
      } else {

        mod <- ranger::ranger(y = y, x = x, probability = TRUE, ...)
      }

      if (y[1] == max(y)) {
        attr(mod, "targ_col") <- 1L
      } else {
        attr(mod, "targ_col") <- 2L
      }
      return(mod)
    } else if (model == "linear") {
      return(glm(y ~ ., data = cbind(y, x), family = "binomial", ...))
    }

  } else {

    # regression case
    if (model == "ranger") {

      if (tune_params & is.null(mns)) {

        mod <- tune_params_ranger(y, x, "regr")
      } else {

        mod <- ranger::ranger(y = y, x = x, ...)
      }

      return(mod)
    } else if (model == "linear") {

      return(lm(y ~ ., data = cbind(y, x), ...))
    }

  }

}

pred <- function(mod, x, ...) {

  if (is.numeric(mod)) {

    assertthat::assert_that(is.null(unlist(x)))
    return(rep(mod, nrow(x)))
  }
  else if (inherits(mod, "glm")) {

    return(predict(mod, x, type = "response"))
  } else if (inherits(mod, "lm")) {

    return(predict(mod, x, ...))
  } else if (inherits(mod, "ranger")) {

    #preds <- predict(mod, x)
    if (mod$treetype == "Probability estimation") {

      preds <- predict(mod, x, ...)$predictions[, attr(mod, "targ_col")]
      return(preds)
    } else {

      return(predict(mod, x, ...)$predictions)
    }

  }

}

doubly_robust_med <- function(x, z, w, y, K = 5, model = "ranger", tune_params,
                              params, extrm_prob = 0.01, ...) {

  if (is.factor(x)) x <- as.integer(x) - 1L
  if (is.factor(y)) {

    assertthat::assert_that(length(unique(y)) == 2L)
    y <- as.integer(y) - 1L
  }

  assertthat::assert_that(is.vector(x) | is.factor(x), is.vector(y) | is.factor(y),
                          msg = "Attribute or outcome is not a vector. Disallowed.")

  if (is.vector(z) | is.factor(w)) z <- data.frame(z = z)
  if (is.vector(w) | is.factor(w)) w <- data.frame(w = w)

  folds <- sample(as.integer(cut(seq_along(x), breaks = K)), replace = FALSE)
  y0 <- y1 <- y0w1 <- y1w0 <- px_z <- px_zw <- rep(NA_real_, length(x))

  for (k in seq_len(K)) {

    ts <- folds == k
    tr <- folds != k

    # total effects

    # regress X on Z
    px_z_tr <- regr(x[tr], z[tr, , drop = FALSE], model = model,
                    mns = params[["mns_pxz"]], tune_params = tune_params, ...)
    if (is.list(px_z_tr)) params[["mns_pxz"]] <- px_z_tr$min.node.size

    # regress Y on Z for each level
    y_z0_tr <- regr(y[tr & x == 0], cbind(z[tr & x == 0, , drop = FALSE]),
                    model = model, mns = params[["mns_yz"]],
                    tune_params = tune_params, ...)
    if (is.list(y_z0_tr)) params[["mns_yz"]] <- y_z0_tr$min.node.size

    y_z1_tr <- regr(y[tr & x == 1], cbind(z[tr & x == 1, , drop = FALSE]),
                    model = model, mns = params[["mns_yz"]],
                    tune_params = tune_params, ...)

    # make predictions on the target partition
    px_z_ts <- pred(px_z_tr, z[ts, , drop = FALSE], ...)
    px_z[ts] <- px_z_ts

    y_z0_ts <- pred(y_z0_tr, cbind(z[ts, , drop = FALSE]), ...)
    y_z1_ts <- pred(y_z1_tr, cbind(z[ts, , drop = FALSE]), ...)

    y0[ts] <- (y[ts] - y_z0_ts) * (x[ts] == 0) / (1 - px_z_ts) +
      y_z0_ts
    y1[ts] <- (y[ts] - y_z1_ts) * (x[ts] == 1) / (px_z_ts) +
      y_z1_ts

    # regress X on Z + W
    px_zw_tr <- regr(x[tr], cbind(z, w)[tr, , drop = FALSE], model = model,
                     mns = params[["mns_pxzw"]], tune_params = tune_params, ...)
    if (is.list(px_zw_tr)) params[["mns_pxzw"]] <- px_zw_tr$min.node.size

    # predict the regression on target partition
    px_zw_ts <- pred(px_zw_tr, cbind(z, w)[ts, , drop = FALSE], ...)
    px_zw[ts] <- px_zw_ts

    # split complement into two equal parts

    # part 1: learn the mean
    mu <- seq_along(x) %in% sample(which(tr), sum(tr) / 2)
    ns <- tr & !mu

    # regress Y ~ Z + W for each level of x
    y_zw0_mu <- regr(y[mu & x == 0], cbind(z[mu & x == 0, , drop = FALSE],
                                           w[mu & x == 0, , drop = FALSE]),
                     model = model, mns = params[["mns_yzw"]],
                     tune_params = tune_params, ...)
    if (is.list(y_zw0_mu)) params[["mns_yzw"]] <- y_zw0_mu$min.node.size

    y_zw1_mu <- regr(y[mu & x == 1], cbind(z[mu & x == 1, , drop = FALSE],
                                           w[mu & x == 1, , drop = FALSE]),
                     model = model, mns = params[["mns_yzw"]],
                     tune_params = tune_params, ...)

    # part 2: learn nested mean

    # predict on nested partition using mu
    y_zw0_ns <- pred(y_zw0_mu, cbind(z[ns, , drop = FALSE],
                                     w[ns, , drop = FALSE]), ...)
    y_zw1_ns <- pred(y_zw1_mu, cbind(z[ns, , drop = FALSE],
                                     w[ns, , drop = FALSE]), ...)

    # learn the nested mean function on nested partition
    ey_zw1_0_ns <- regr(y_zw1_ns[x[ns] == 0], z[ns & x == 0, , drop = FALSE],
                        model = model, mns = params[["mns_eyzw"]],
                        tune_params = tune_params, ...)
    if (is.list(ey_zw1_0_ns)) params[["mns_eyzw"]] <- y_zw0_mu$min.node.size

    ey_zw0_1_ns <- regr(y_zw0_ns[x[ns] == 1], z[ns & x == 1, , drop = FALSE],
                        model = model, mns = params[["mns_eyzw"]],
                        tune_params = tune_params, ...)

    # part 3: compute the mean / nested mean on target partition
    y_zw0_ts <- pred(y_zw0_mu, cbind(z[ts, , drop = FALSE],
                                     w[ts, , drop = FALSE]), ...)
    ey_zw0_1_ts <- pred(ey_zw0_1_ns, z[ts, , drop = FALSE], ...)

    y_zw1_ts <- pred(y_zw1_mu, cbind(z[ts, , drop = FALSE],
                                     w[ts, , drop = FALSE]), ...)
    ey_zw1_0_ts <- pred(ey_zw1_0_ns, z[ts, , drop = FALSE], ...)

    # part 4: compute the formula

    y0w1[ts] <-
      (px_zw_ts) * (x[ts] == 0) / ((1 - px_zw_ts) * px_z_ts) *
      (y[ts] - y_zw0_ts) +
      (x[ts] == 1) / (1 - px_z_ts) * (y_zw0_ts - ey_zw0_1_ts) +
      ey_zw0_1_ts

    y1w0[ts] <-
      (1 - px_zw_ts) * (x[ts] == 1) / ((px_zw_ts) * (1 - px_z_ts)) *
      (y[ts] - y_zw1_ts) +
      (x[ts] == 0) / (px_z_ts) * (y_zw1_ts - ey_zw1_0_ts) +
      ey_zw1_0_ts
  }

  # trim the extreme probabilities
  extrm_pxz <- (px_z < extrm_prob) | (1 - px_z < extrm_prob)
  extrm_pxzw <-  (px_zw < extrm_prob) | (1 - px_zw < extrm_prob)
  extrm_idx <- extrm_pxz | extrm_pxzw

  y0[extrm_idx] <- y1[extrm_idx] <- y0w1[extrm_idx] <- y1w0[extrm_idx] <- NA

  if (mean(extrm_idx) > 0.02) {
    message(100 * mean(extrm_idx),
            "% of extreme P(x | z) or P(x | z, w) probabilities.\n",
            "Estimates likely biased.")
  }

  return(list(y0, y1, y0w1, y1w0, params = params))
}

ci_mdml <- function(data, X, Z, W, Y, x0, x1, model, rep, nboot,
                    tune_params, params) {

  boot_samp <- if (rep > 1) sample.int(nrow(data), replace = TRUE) else
                            seq_len(nrow(data))
  boot_data <- data[boot_samp, ]

  boots <- lapply(
    seq_len(nboot),
    function(i) {

      ind <- sample.int(nrow(boot_data), replace = TRUE)
      idx0 <- boot_data[[X]][ind] == x0
      list(all = ind, id0 = ind[idx0], id1 = ind[!idx0])
    }
  )

  y <- as.numeric(boot_data[[Y]]) - is.factor(boot_data[[Y]])
  tv <- msd_two(y, "id1", -y, "id0", "tv", boots)

  if (length(c(Z, W)) > 0) {

    est_med <- doubly_robust_med(boot_data[[X]], boot_data[, Z], boot_data[, W],
                                 boot_data[[Y]], model = model,
                                 tune_params = tune_params, params = params)
    params <- est_med[["params"]]
    yx0 <- est_med[[1]]
    yx1 <- est_med[[2]]
    yx1wx0 <- est_med[[4]]
  }

  if (length(Z) == 0) {

    te <- inh_str(tv, "te")
    ett <- inh_str(tv, "ett")
    expse_x1 <- inh_str(tv, "expse_x1", set0 = TRUE)
    expse_x0 <- inh_str(tv, "expse_x0", set0 = TRUE)
    ctfse <- inh_str(tv, "ctfse", set0 = TRUE)
  } else {

    # get TE/ETT
    te <- msd_two(yx1, "all", -yx0, "all", "te", boots) # pyx1 - pyx0
    ett <- msd_two(yx1, "id0", -y, "id0", "ett", boots) # pyx1_x0 - py_x0

    # get SE
    ctfse <- msd_two(yx1, "id0", -y, "id1", "ctfse", boots)# Ctf-SE_{x_1, x_0}(y)=pyx1_x0-py_x1
    expse_x1 <- msd_two(y, "id1", -yx1, "all", "expse_x1", boots) # py_x1 - pyx1
    expse_x0 <- msd_two(y, "id0", -yx0, "all", "expse_x0", boots) # py_x0 - pyx0
  }

  if (length(W) == 0) {

    nde <- inh_str(te, "nde")
    ctfde <- inh_str(ett, "ctfde")
    ctfie <- inh_str(ett, "ctfie", set0 = TRUE)
    nie <- inh_str(te, "nie", set0 = TRUE)
  } else {

    # get DE
    nde <- msd_two(yx1wx0, "all", -yx0, "all", "nde", boots) # NDE_{x_0, x_1}(y)
    ctfde <- msd_two(yx1wx0, "id0", -y, "id0", "ctfde", boots) #Ctf-DE_{x_0, x_1}(y|x_0)

    # get IE
    nie <- msd_two(yx1wx0, "all", -yx1, "all", "nie", boots) # NIE_{x_1, x_0}(y)
    ctfie <- msd_two(yx1wx0, "id0", -yx1, "id0", "ctfie", boots)#Ctf-IE_{x_1, x_0}(y|x0)
  }

  res <- do.call(
    rbind,
    list(tv, te, expse_x1, expse_x0, ett, ctfse, nde, nie, ctfde, ctfie)
  )
  res <- data.frame(res, rep = rep)
  attr(res, "params") <- params
  res
}
