
ex_med_bias <- function(n, type = "dat") {

  expit <- function(x) exp(x) / (1 + exp(x))

  Z <- replicate(3, rnorm(n))
  colnames(Z) <- paste0("Z", 1:3)

  X <- rbinom(n, size = 1, prob = expit(0.5 * rowMeans(Z)))

  fW1 <- function(X, Z, eps) X * (rowMeans(Z[, c(1, 2)]) - 1) + eps
  fW2 <- function(X, Z, W1, eps) W1^2 / 2 - 1 +
    X * (1 / 2 * rowMeans((Z^2)[, c(2, 3)]) - 1) +
    eps
  fW3 <- function(X, Z, W1, W2, eps) W1 * W2 / 6 + X * (Z[, 1] / 4 - 2) + eps

  eps_w1 <- rnorm(n)
  eps_w2 <- rnorm(n)
  eps_w3 <- rnorm(n)

  W1 <- fW1(X, Z, eps_w1)
  W2 <- fW2(X, Z, W1, eps_w2)
  W3 <- fW3(X, Z, W1, W2, eps_w3)

  f0 <- function(x) 1 / 2 * rowMeans(abs(x))
  f1 <- function(x) rowSums((1 / 3 * x^2 * max(1, 1/2 * log(abs(x))))[, c(T, F)])

  if (type == "dat") {
    Y <- f0(cbind(Z, W1, W2, W3)) + X * f1(cbind(Z, W1, W2, W3)) +
      rnorm(n, sd = 1/2)
    dat <- data.frame(cbind(X, Z, W1, W2, W3, Y))
    return(dat)
  } else if (type == "gtruth") {
    # potential outcomes
    W1_0 <- fW1(0, Z, eps_w1)
    W2_0 <- fW2(0, Z, W1_0, eps_w2)
    W3_0 <- fW3(0, Z, W1_0, W2_0, eps_w3)

    W1_1 <- fW1(1, Z, eps_w1)
    W2_1 <- fW2(1, Z, W1_1, eps_w2)
    W3_1 <- fW3(1, Z, W1_1, W2_1, eps_w3)

    y <- f0(cbind(Z, W1, W2, W3)) + X * f1(cbind(Z, W1, W2, W3))
    yx1wx0 <- f0(cbind(Z, W1_0, W2_0, W3_0)) +
      1 * f1(cbind(Z, W1_0, W2_0, W3_0))
    yx1 <- f0(cbind(Z, W1_1, W2_1, W3_1)) + 1 * f1(cbind(Z, W1_1, W2_1, W3_1))
    yx0 <- f0(cbind(Z, W1_0, W2_0, W3_0)) + 0 * f1(cbind(Z, W1_0, W2_0, W3_0))

    de <- yx1wx0 - yx0
    ie <- yx1wx0 - yx1

    return(
      list(
        nde = mean(de),
        ctfde = mean(de[X == 0]),
        te = mean(yx1 - yx0),
        nie = mean(ie),
        ctfie = mean(ie[X == 0]),
        ett = mean((yx1 - yx0)[X == 0]),
        ctfse = mean(yx1[X == 0]) - mean(y[X == 1]),
        expse_x1 = mean(y[X == 1]) - mean(yx1),
        expse_x0 = mean(y[X == 0]) - mean(yx0),
        tv = mean(y[X == 1]) - mean(y[X == 0])
      )
    )
  }

}

ex_med_spur <- function(n, type = "dat") {

  expit <- function(x) exp(x) / (1 + exp(x))

  Z <- replicate(3, rnorm(n))
  colnames(Z) <- paste0("Z", 1:3)

  X <- rbinom(n, size = 1, prob = 0.5)
  Z <- Z + rbinom(n, 1, 0.2 + 0.6 * X) * 1

  fW1 <- function(X, Z, eps, uw)
    (uw < 0.2 + X * 0.6) * (rowMeans(Z[, c(1, 2)]) - 1) + eps
  fW2 <- function(X, Z, W1, eps, uw) W1^2 / 8 - 1 +
    (uw < 0.3 + X * 0.6) * (1 / 2 * rowMeans((Z^2)[, c(2, 3)]) - 1) + eps
  fW3 <- function(X, Z, W1, W2, eps, uw) W1 * W2 / 12 +
    (uw < 0.2 + X * 0.6) * (Z[, 1] / 4 - 2) +
    eps

  eps_w1 <- rnorm(n)
  eps_w2 <- rnorm(n)
  eps_w3 <- rnorm(n)
  u_w1 <- runif(n)
  u_w2 <- runif(n)
  u_w3 <- runif(n)

  W1 <- fW1(X, Z, eps_w1, u_w1)
  W2 <- fW2(X, Z, W1, eps_w2, u_w2)
  W3 <- fW3(X, Z, W1, W2, eps_w3, u_w3)

  f0 <- function(x) 1 / 8 * rowMeans(x^2 * sign(x))
  f1 <- function(x) rowSums((1 / 5 * x^2 * sign(x) * max(1, 1/2 * log(abs(x),10)))[, c(T, F)])

  if (type == "dat") {
    Y <- f0(cbind(Z, W1, W2, W3)) + X * f1(cbind(Z, W1, W2, W3)) +
      rnorm(n, sd = 1/2)
    dat <- data.frame(cbind(X, Z, W1, W2, W3, Y))
    return(dat)
  } else if (type == "gtruth") {

    # potential outcomes
    W1_0 <- fW1(0, Z, eps_w1, u_w1)
    W2_0 <- fW2(0, Z, W1_0, eps_w2, u_w2)
    W3_0 <- fW3(0, Z, W1_0, W2_0, eps_w3, u_w3)

    W1_1 <- fW1(1, Z, eps_w1, u_w1)
    W2_1 <- fW2(1, Z, W1_1, eps_w2, u_w2)
    W3_1 <- fW3(1, Z, W1_1, W2_1, eps_w3, u_w3)

    y <- f0(cbind(Z, W1, W2, W3)) + X * f1(cbind(Z, W1, W2, W3))
    yx1wx0 <- f0(cbind(Z, W1_0, W2_0, W3_0)) +
      1 * f1(cbind(Z, W1_0, W2_0, W3_0))
    yx1 <- f0(cbind(Z, W1_1, W2_1, W3_1)) + 1 * f1(cbind(Z, W1_1, W2_1, W3_1))
    yx0 <- f0(cbind(Z, W1_0, W2_0, W3_0)) + 0 * f1(cbind(Z, W1_0, W2_0, W3_0))

    de <- yx1wx0 - yx0
    ie <- yx1wx0 - yx1

    return(
      list(
        nde = mean(de),
        ctfde = mean(de[X == 0]),
        te = mean(yx1 - yx0),
        nie = mean(ie),
        ctfie = mean(ie[X == 0]),
        ett = mean((yx1 - yx0)[X == 0]),
        ctfse = mean(yx1[X == 0]) - mean(y[X == 1]),
        expse_x1 = mean(y[X == 1]) - mean(yx1),
        expse_x0 = mean(y[X == 0]) - mean(yx0),
        tv = mean(y[X == 1]) - mean(y[X == 0])
      )
    )
  }

}

ex_med <- function(n, type = "dat") {

  expit <- function(x) exp(x) / (1 + exp(x))

  Z <- replicate(3, rnorm(n))
  colnames(Z) <- paste0("Z", 1:3)

  X <- rbinom(n, size = 1, prob = expit(0.5 * rowMeans(Z)))

  fW1 <- function(X, Z, eps, uw)
    (uw < 0.2 + X * 0.6) * (rowMeans(Z[, c(1, 2)]) - 1) + eps
  fW2 <- function(X, Z, W1, eps, uw) W1^2 / 8 - 1 +
    (uw < 0.3 + X * 0.6) * (1 / 6 * rowMeans((Z^2)[, c(2, 3)]) - 1) + eps
  fW3 <- function(X, Z, W1, W2, eps, uw) W1 * W2 / 16 +
    (uw < 0.2 + X * 0.6) * (Z[, 1] / 4 - 2) +
    eps

  eps_w1 <- rnorm(n)
  eps_w2 <- rnorm(n)
  eps_w3 <- rnorm(n)
  u_w1 <- runif(n)
  u_w2 <- runif(n)
  u_w3 <- runif(n)

  W1 <- fW1(X, Z, eps_w1, u_w1)
  W2 <- fW2(X, Z, W1, eps_w2, u_w2)
  W3 <- fW3(X, Z, W1, W2, eps_w3, u_w3)

  f0 <- function(x) 1 / 4 * rowMeans(abs(x))
  f1 <- function(x) rowSums((1 / 6 * x^2 * max(1, 1/2 * log(abs(x))))[, c(T, F)])

  if (type == "dat") {
    Y <- f0(cbind(Z, W1, W2, W3)) + X * f1(cbind(Z, W1, W2, W3)) +
      rnorm(n, sd = 1/2)
    dat <- data.frame(cbind(X, Z, W1, W2, W3, Y))
    return(dat)
  } else if (type == "gtruth") {

    # potential outcomes
    W1_0 <- fW1(0, Z, eps_w1, u_w1)
    W2_0 <- fW2(0, Z, W1_0, eps_w2, u_w2)
    W3_0 <- fW3(0, Z, W1_0, W2_0, eps_w3, u_w3)

    W1_1 <- fW1(1, Z, eps_w1, u_w1)
    W2_1 <- fW2(1, Z, W1_1, eps_w2, u_w2)
    W3_1 <- fW3(1, Z, W1_1, W2_1, eps_w3, u_w3)

    y <- f0(cbind(Z, W1, W2, W3)) + X * f1(cbind(Z, W1, W2, W3))
    yx1wx0 <- f0(cbind(Z, W1_0, W2_0, W3_0)) +
      1 * f1(cbind(Z, W1_0, W2_0, W3_0))
    yx1 <- f0(cbind(Z, W1_1, W2_1, W3_1)) + 1 * f1(cbind(Z, W1_1, W2_1, W3_1))
    yx0 <- f0(cbind(Z, W1_0, W2_0, W3_0)) + 0 * f1(cbind(Z, W1_0, W2_0, W3_0))

    de <- yx1wx0 - yx0
    ie <- yx1wx0 - yx1

    return(
      list(
        nde = mean(de),
        ctfde = mean(de[X == 0]),
        te = mean(yx1 - yx0),
        nie = mean(ie),
        ctfie = mean(ie[X == 0]),
        ett = mean((yx1 - yx0)[X == 0]),
        ctfse = mean(yx1[X == 0]) - mean(y[X == 1]),
        expse_x1 = mean(y[X == 1]) - mean(yx1),
        expse_x0 = mean(y[X == 0]) - mean(yx0),
        tv = mean(y[X == 1]) - mean(y[X == 0])
      )
    )
  }

}

ex_nomed <- function(n, type = "dat") {

  f0 <- function(x) rowSums(x)
  f1 <- function(x) rowSums((x^2)[, c(T, F)])
  expit <- function(x) exp(x) / (1 + exp(x))

  Z <- replicate(3, runif(n, -1, 1))
  colnames(Z) <- paste0("Z", seq_len(3))

  X <- rbinom(n, size = 1, prob = expit(1 / 2 * rowSums(Z)))
  Y <- f0(Z) + X * f1(Z) + rnorm(n, sd = 1/2)

  dat <- data.frame(cbind(X, Z, Y))

  if (type == "dat") {
    return(dat)
  } else if (type == "gtruth") {

    y <- f0(Z) + X * f1(Z)
    y_1 <- f0(Z) + f1(Z)
    y_0 <- f0(Z)
    te <- de <- y_1 - y_0
    ie <- rep(0, length(de))

    return(
      list(
        nde = mean(de),
        ctfde = mean(de[X == 0]),
        te = mean(de - ie),
        nie = mean(ie),
        ctfie = mean(ie[X == 0]),
        ett = mean((de-ie)[X == 0]),
        ctfse = mean(y_1[X == 0]) - mean(y[X == 1]),
        expse_x1 = mean(y[X == 1]) - mean(y_1),
        expse_x0 = mean(y[X == 0]) - mean(y_0),
        tv = mean(y[X == 1]) - mean(y[X == 0])
      )
    )
  }

}

vis_diff <- function(res, measure = c("CtfDE", "ETT", "ExpSE_x0", "ExpSE_x1",
                                      "CtfIE", "NDE", "NIE", "CtfSE", "TE")) {
  #browser()
  res <- res[res$measure %in% measure, ]
  res <- as.data.table(res)
  res[, faircause_rng := mean(value[method == "faircause_ranger"]),
      by = "measure"]
  res[, faircause_lnr := mean(value[method == "faircause_linear"]),
      by = "measure"]
  p <- ggplot(res, aes(x = jitter(value), fill = method)) +
    geom_density(alpha = 0.5) +
    theme_bw() + xlab("Estimate") +
    ylab("Density") + theme(
      legend.position = "bottom",
      legend.box.background = element_rect(),
    ) +
    facet_wrap(vars(measure), scales = "free", ncol = 1L,
               strip.position = "right")

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  mthd <- sort(unique(res$method))
  clrs <- gg_color_hue(length(mthd))

  if ("ground_truth" %in% names(res)) {
    p <- p + geom_vline(aes(xintercept = ground_truth), color = "red",
                        linetype = "dashed", size = 1)
  }

  if ("faircause_ranger" %in% res$method) {
    p <- p + geom_vline(aes(xintercept = faircause_rng),
                        color = clrs[mthd == "faircause_ranger"])
  }
  if ("faircause_linear" %in% res$method) {
    p <- p + geom_vline(aes(xintercept = faircause_lnr),
                        color = clrs[mthd == "faircause_linear"])
  }

  p
}

check_constraints <- function(x, mthd = "faircause_ranger") {

  x <- as.data.table(x)
  x <- x[method == mthd]

  # constraint 1: TE = NDE + NIE
  c1 <- x[measure %in% c("TE", "NDE", "NIE"),
          c("value", "measure", "boot_num"),
          with = F]
  c1 <- dcast(c1, boot_num ~ measure)
  c1[, TE_tot := NDE / TE - NIE / TE]

  # constraint 2: TV = TE + ExpSE_x1 - ExpSE_x0
  c2 <- x[measure %in% c("TV", "TE", "ExpSE_x1", "ExpSE_x0"),
          c("value", "measure", "boot_num"),
          with = F]
  c2 <- dcast(c2, boot_num ~ measure)
  c2[, TV_tot := TE/TV + ExpSE_x1/TV - ExpSE_x0/TV]

  # constraint 3: ETT = DE + IE
  c3 <- x[measure %in% c("ETT", "CtfDE", "CtfIE"),
          c("value", "measure", "boot_num"),
          with = F]
  c3 <- dcast(c3, boot_num ~ measure)
  c3[, ETT_tot := CtfDE / ETT - CtfIE / ETT]

  # constraint 4: TV = ETT - Ctf-SE
  c4 <- x[measure %in% c("TV", "ETT", "CtfSE"),
          c("value", "measure", "boot_num"),
          with = F]
  c4 <- dcast(c4, boot_num ~ measure)
  c4[, TV2_tot := ETT / TV - CtfSE / TV]

  dat <- cbind(c1$TE_tot, c2$TV_tot, c3$ETT_tot, c4$TV2_tot, c1$boot_num)
  dat <- data.table(dat)
  names(dat) <- c("TE = NDE + NIE", "TV = TE + ExpSE_x1 - ExpSE_x0",
                  "ETT = Ctf-DE + Ctf-IE", "TV = ETT - Ctf-SE", "boot_num")

  dat <- melt(dat, id.vars = "boot_num")
  dat$value <- dat$value + rnorm(length(dat$value), sd = 0.01)

  ggplot(dat, aes(x = value, fill = factor(variable))) +
    geom_density(alpha = 0.8) +
    theme_bw() +
    scale_fill_discrete(name = "Constraint") +
    theme(
      legend.position = c(0.8, 0.8),
      legend.box.background = element_rect()
    ) +
    geom_vline(xintercept = 1, color = "red", linetype = "dashed", size = 1.5)

  #' * TeX(paste(phi_poly, "=", theta_poly)) *

}

method_cmp <- function(measure = c("CtfDE", "ETT", "ExpSE_x0", "ExpSE_x1",
                                   "CtfIE", "NDE", "NIE", "CtfSE", "TE"),
                       example = "med", nboot = 5, nsamp = 2000,
                       model = "ranger") {

  if (example == "nomed") {
    gt <- ex_nomed(10^5, "gtruth")
  } else if (example == "med") {
    gt <- ex_med(10^5, "gtruth")
  } else if (example == "berkeley") {
    gt <- ex_berk_gt()
  } else gt <- NULL

  est <- NULL
  for(i in seq_len(nboot)) {

    # get the example
    ex <- get_ex(example, nsamp = nsamp, boot_num = i)

    # apply the cookbook
    cb <- c()
    for (mod in model) {
      cbook <- fairness_cookbook(ex$dat, ex$X, ex$W, ex$Z, ex$Y, 0, 1,
                                 model = mod)
      cb <- rbind(
        cb,
        data.frame(measure = names(cbook$measures),
                   value = vapply(cbook$measures,
                                  function(x) x[1], numeric(1L)),
                   method = paste0("faircause_", mod))
      )
    }


    # apply causal forest
    if (any(c("TE", "ETT") %in% measure)) {
      cf <- cforest(ex)
    }

    # apply causalweight
    if (any(c("NDE", "NIE", "TE") %in% measure)) {
      cw <- cweight(ex)
    }

    if ("TE" %in% measure) {
      dre <- dre(ex)
    }

    add <- rbind(cb, cf, cw, dre)
    add <- cbind(add, boot_num = i)
    est <- rbind(est, add)
  }

  if (!is.null(gt)) {
    gt <- data.frame(measure = names(gt), ground_truth = unlist(gt))
    est <- merge(est, gt, by = "measure", all.x = TRUE)
  }

  est

}

cforest <- function(ex) {

  if (ncol(ex$dat[, ex$Z]) == 0L) {
    te <- mean(ex$dat[[ex$Y]][ex$dat[[ex$X]] == 1]) -
      mean(ex$dat[[ex$Y]][ex$dat[[ex$X]] == 0])
    qnt <- c(te, te, te)
    return(data.frame(
      measure = c("TE", "ETT", "ETU"), value = qnt, method = "causal_forest"
    ))
  }

  crf <- causal_forest(
    X = ex$dat[, ex$Z],
    Y = ex$dat[[ex$Y]],
    W = ex$dat[[ex$X]],
    num.threads = n_cores()
  )

  qnt <- c(
    mean(crf$predictions), # TE estimate
    mean(crf$predictions[ex$dat[[ex$X]] == 0]), # ETT
    mean(crf$predictions[ex$dat[[ex$X]] == 1]) # ETU (!)
  )

  data.frame(
    measure = c("TE", "ETT", "ETU"), value = qnt, method = "causal_forest"
  )

}

cweight <- function(ex) {

  if (ncol(ex$dat[, ex$Z]) == 0L) {
    x <- cbind(z1 = rnorm(nrow(ex$dat)), z2 = rnorm(nrow(ex$dat)))
    cwg <- medDML(
      y = ex$dat[[ex$Y]],
      d = ex$dat[[ex$X]],
      m = ex$dat[, ex$W],
      x = x
    )
  } else {
    cwg <- medDML(
      y = ex$dat[[ex$Y]],
      d = ex$dat[[ex$X]],
      m = ex$dat[, ex$W],
      x = ex$dat[, ex$Z]
    )
  }

  data.frame(
    measure = c("TE", "NDE", "NIE"),
    value = c(cwg$results["effect", "total"],
              cwg$results["effect", "dir.treat"],
              -cwg$results["effect", "indir.control"]),
    method = "causalweight"
  )
}

dre <- function(ex) {

  if (ncol(ex$dat[, ex$Z]) == 0L) {
    te <- mean(ex$dat[[ex$Y]][ex$dat[[ex$X]] == 1]) -
      mean(ex$dat[[ex$Y]][ex$dat[[ex$X]] == 0])
    qnt <- c(te)
    return(data.frame(
      measure = c("TE"), value = qnt, method = "DR"
    ))
  }

  dat0 <- dat1 <- dat <- ex$dat
  dat0$X <- 0
  dat1$X <- 1
  n <- nrow(dat)

  mu1 <- model_mean(as.formula(paste0("Y ~ X + ",
                                      paste0(ex$Z, collapse = "+"))),
                    dat, dat1,
                    probability = length(unique(ex$dat[[ex$Y]])) == 2L)
  mu0 <- model_mean(as.formula(paste0("Y ~ X + ",
                                      paste0(ex$Z, collapse = "+"))),
                    dat, dat0,
                    probability = length(unique(ex$dat[[ex$Y]])) == 2L)

  prop1 <- model_propensity(
    as.formula(paste0("X ~ ", paste0(ex$Z, collapse = "+"))), ex$dat, xlvl = 1)

  idx <- ex$dat[[ex$X]] == 1
  y1 <- (ex$dat[[ex$Y]] - mu1) * idx / prop1 + mu1
  y0 <- (ex$dat[[ex$Y]] - mu0) * (!idx) / (1 - prop1) + mu0

  DR_ATE <- mean((y1 - y0))
  DR_ETT <- mean((y1 - y0)[!idx])
  DR_ETU <- mean((y1 - y0)[idx])
  data.frame(measure = c("TE", "ETT", "ETU"),
             value = c(DR_ATE, DR_ETT, DR_ETU), method = "DR")
}

get_ex <- function(example, nsamp = 1000, boot_num = 1L, gtruth = FALSE) {

  if (example == "nomed") {
    dat <- ex_nomed(nsamp, seed = 2022 + boot_num)
  } else if (example == "med") {
    dat <- ex_med(nsamp, seed = 2022 + boot_num)
  } else if (example == "compas") {
    dat <- get_data("compas")
    dat$sex <- as.integer(dat$sex) - 1L
    dat$c_charge_degree <- 2L - as.integer(dat$c_charge_degree)
    meta <- get_metadata("compas")

    # rename cols
    dat <- setnames(dat, meta$X, "X")
    dat <- setnames(dat, meta$Y, "Y")
    dat <- setnames(dat, meta$Z, paste0("Z", seq_along(meta$Z)))
    dat <- setnames(dat, meta$W, paste0("W", seq_along(meta$W)))

    # attribute values
    dat$X <- as.integer(dat$X) - 1L
    dat <- dat[sample(nrow(dat), replace = TRUE), ]
  } else if (example == "census") {

    dat <- get_data("census")
    dat$sex <- as.integer(dat$sex) - 1L

  } else if (example == "berkeley") {

    dat <- get_data("berkeley")
    meta <- get_metadata("berkeley")
    dat$admit <- as.integer(dat$admit) - 1L
    dat$dept <- as.integer(dat$dept)
    dat$gender <- as.integer(dat$gender) - 1L

    dat <- setnames(dat, meta$X, "X")
    dat <- setnames(dat, meta$Y, "Y")
    dat <- setnames(dat, meta$W, "W")

  }

  list(
    dat = dat,
    Z = grep("^Z", names(dat), value = TRUE),
    W = grep("^W", names(dat), value = TRUE),
    X = "X", Y = "Y"
  )

}

ex_berk_gt <- function() {

  ex <- get_ex("berkeley")
  pyx1w <- table(ex$dat[ex$dat$X == 1, ]$Y, ex$dat[ex$dat$X == 1, ]$W)
  pyx1w <- pyx1w[2, ] / (pyx1w[1, ] + pyx1w[2, ])
  pwx <- table(ex$dat$W, ex$dat$X)
  pwx0 <- pwx[, "0"] / sum(pwx[, "0"])

  yx1wx0 <- sum(pyx1w * pwx0)
  yx1 <- mean(ex$dat$Y[ex$dat$X == 1])
  yx0 <- mean(ex$dat$Y[ex$dat$X == 0])

  list(
    NDE = yx1wx0 - yx0,
    CtfDE = yx1wx0 - yx0,
    TE = yx1 - yx0,
    NIE = yx1wx0 - yx1,
    CtfIE = yx1wx0 - yx1,
    ETT = yx1 - yx0,
    CtfSE = 0,
    ExpSE_x1 = 0,
    ExpSE_x0 = 0,
    TV = yx1 - yx0
  )
}
