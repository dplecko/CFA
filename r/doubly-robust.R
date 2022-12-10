
#' * could be S3 dispatch in the future *
regr <- function(y, x, model = "ranger", ...) {

  if (is.vector(x)) x <- data.frame(x = x)

  # Z set empty -> x equals NULL -> return just the mean
  if (is.null(unlist(x))) {
    if (!is.logical(y) & !is.numeric(y)) browser()
    return(mean(y))
  }

  if (setequal(unique(y), c(0, 1))) {

    # probability case
    if (model == "ranger") {
      #cat("First value", y[1], "\n")
      mod <- ranger::ranger(y = y, x = x, probability = TRUE, ...)
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
      return(ranger::ranger(y = y, x = x, ...))
    } else if (model == "linear") {
      return(lm(y ~ ., data = cbind(y, x), ...))
    }

  }

}

#' * could be S3 dispatch in the future *
pred <- function(mod, x, ...) {

  if (is.vector(x)) x <- data.frame(x = x)

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

doubly_robust_med <- function(x, z, w, y, K = 5, model = "ranger",
                              extrm_prob = 0.01, ...) {

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
    px_z_tr <- regr(x[tr], z[tr, , drop = FALSE], model = model, ...)

    # regress Y on Z for each level
    y_z0_tr <- regr(y[tr & x == 0], cbind(z[tr & x == 0, , drop = FALSE]),
                    model = model, ...)
    y_z1_tr <- regr(y[tr & x == 1], cbind(z[tr & x == 1, , drop = FALSE]),
                    model = model, ...)

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
    px_zw_tr <- regr(x[tr], cbind(z, w)[tr, , drop = FALSE], model = model, ...)

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
                     model = model, ...)
    y_zw1_mu <- regr(y[mu & x == 1], cbind(z[mu & x == 1, , drop = FALSE],
                                           w[mu & x == 1, , drop = FALSE]),
                     model = model, ...)

    # part 2: learn nested mean

    # predict on nested partition using mu
    y_zw0_ns <- pred(y_zw0_mu, cbind(z[ns, , drop = FALSE],
                                     w[ns, , drop = FALSE]), ...)
    y_zw1_ns <- pred(y_zw1_mu, cbind(z[ns, , drop = FALSE],
                                     w[ns, , drop = FALSE]), ...)

    # learn the nested mean function on nested partition
    ey_zw1_0_ns <- regr(y_zw1_ns[x[ns] == 0], z[ns & x == 0, , drop = FALSE],
                        model = model, ...)
    ey_zw0_1_ns <- regr(y_zw0_ns[x[ns] == 1], z[ns & x == 1, , drop = FALSE],
                        model = model, ...)

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
            "NDE, NIE, Ctf-DE, Ctf-IE estimates likely biased.")
  }

  return(list(y0, y1, y0w1, y1w0))

}

model_mean <- function(form, data, int.data, Y, ...) {

  rf <- ranger::ranger(form, data = data, keep.inbag = TRUE,
                       importance = "impurity", ...)
  assertthat::assert_that(rf$treetype %in% c("Regression",
                                             "Probability estimation"))

  if (rf$treetype == "Probability estimation") {

    if (is.factor(data[[Y]])) {
      targ_col <- as.character(levels(data[[Y]])[2])
    } else if (is.numeric(data[[Y]])) {
      targ_col <- 2L - (data[[Y]][1] == max(data[[Y]]))
    } else if (is.logical(data[[Y]])) {
      targ_col <- 2L
    }

    p2 <- predict(rf, int.data, predict.all = TRUE, ...)$predictions[, targ_col, ]

  } else {

    p2 <- predict(rf, int.data, predict.all = TRUE, ...)$predictions

  }

  oob.matrix <- Reduce(cbind, lapply(rf$inbag.counts, function(x=i) x == 0))

  rowSums(p2 * oob.matrix) / rowSums(oob.matrix)

}

model_propensity <- function(form, data, Y, xlvl, ...) {

  assertthat::assert_that(length(xlvl) == 1L)
  assertthat::assert_that(is.factor(data[[Y]]),
                          msg = "Attribute needs to be a factor.")

  rf <- ranger::ranger(form, data = data, keep.inbag = TRUE,
                       importance = "impurity", probability = TRUE, ...)
  assertthat::assert_that(rf$treetype == "Probability estimation")

  rf$predictions[, xlvl]

}
