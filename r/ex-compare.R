
ex_med <- function(n, type = "dat", seed = 2022) {
  
  set.seed(seed)
  
  expit <- function(x) exp(x) / (1 + exp(x))
  
  Z <- replicate(3, runif(n, -1, 1))
  colnames(Z) <- paste0("Z", 1:3)
  
  X <- rbinom(n, size = 1, prob = expit(rowMeans(abs(Z))))
  
  eps_w1 <- rnorm(n)
  eps_w2 <- rnorm(n)
  eps_w3 <- rnorm(n)
  
  W1 <- eps_w1 + X * rowMeans(Z[, c(1, 2)])
  W2 <- eps_w2 + W1^2 / 2 - 1 + X * rowMeans((Z^2)[, c(2, 3)])
  W3 <- eps_w3 + W1 * W2 / 6 + rnorm(n) + X * Z[, 1] / 4
  
  W1_0 <- eps_w1 + 0 * rowMeans(Z[, c(1, 2)])
  W2_0 <- eps_w2 + W1^2 / 2 - 1 + 0 * rowMeans((Z^2)[, c(2, 3)])
  W3_0 <- eps_w3 + W1 * W2 / 6 + rnorm(n) + 0 * Z[, 1] / 4
  
  W1_1 <- eps_w1 + 1 * rowMeans(Z[, c(1, 2)])
  W2_1 <- eps_w2 + W1^2 / 2 - 1 + 1 * rowMeans((Z^2)[, c(2, 3)])
  W3_1 <- eps_w3 + W1 * W2 / 6 + rnorm(n) + 1 * Z[, 1] / 4
  
  f0 <- \(x) rowMeans(abs(x))
  f1 <- \(x) rowSums((x^2 * max(1, log(abs(x))))[, c(T, F)])
  
  if (type == "dat") {
    Y <- f0(cbind(Z, W1, W2, W3)) + X * f1(cbind(Z, W1, W2, W3)) + 
      rnorm(n, sd = 1/2)
    dat <- data.frame(cbind(X, Z, W1, W2, W3, Y))
    return(dat)
  } else if (type == "gtruth") {
    
    de <- f1(cbind(Z, W1, W2, W3))
    ie <- f0(cbind(Z, W1_0, W2_0, W3_0)) - f0(cbind(Z, W1_1, W2_1, W3_1)) +
      f1(cbind(Z, W1_0, W2_0, W3_0)) - f1(cbind(Z, W1_1, W2_1, W3_1))
    
    te <- de - ie
    
    y <- f0(cbind(Z, W1, W2, W3)) + X * f1(cbind(Z, W1, W2, W3))
    y_1 <- f0(cbind(Z, W1_1, W2_1, W3_1)) + 1 * f1(cbind(Z, W1_1, W2_1, W3_1))
    y_0 <- f0(cbind(Z, W1_0, W2_0, W3_0)) + 0 * f1(cbind(Z, W1_0, W2_0, W3_0))
    
    return(
      list(
        NDE = mean(de),
        DE = mean(de[X == 0]),
        TE = mean(de - ie),
        NIE = mean(ie),
        IE = mean(ie[X == 0]),
        ETT = mean((de-ie)[X == 0]),
        SE = mean(y[X == 1]) - mean(y_1[X == 0]),
        ExpSE_x1 = mean(y[X == 1]) - mean(y_1),
        ExpSE_x0 = mean(y[X == 0]) - mean(y_0),
        TV = mean(y[X == 1]) - mean(y[X == 0])
      )
    )
  }
  
}

ex_nomed <- function(n, type = "dat", seed = 2022) {
  
  set.seed(seed)
  
  f0 <- function(x) rowSums(x)
  f1 <- function(x) rowSums((x^2)[, c(T, F)])
  expit <- function(x) exp(x) / (1 + exp(x))
  
  Z <- replicate(3, runif(n, -1, 1))
  colnames(Z) <- paste0("Z", seq_len(3))
  
  X <- rbinom(n, size = 1, prob = expit(1 / 3 * rowSums(Z)))
  
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
        NDE = mean(de),
        DE = mean(de[X == 0]),
        TE = mean(de - ie),
        NIE = mean(ie),
        IE = mean(ie[X == 0]),
        ETT = mean((de-ie)[X == 0]),
        SE = mean(y[X == 1]) - mean(y_1[X == 0]),
        ExpSE_x1 = mean(y[X == 1]) - mean(y_1),
        ExpSE_x0 = mean(y[X == 0]) - mean(y_0),
        TV = mean(y[X == 1]) - mean(y[X == 0])
      )
    )
  }
  
}

vis_diff <- function(res, measure = c("TE", "NDE", "NIE")) {
  
  res <- res[res$measure %in% measure, ]
  
  p <- ggplot(res, aes(x = value, fill = method)) +
    geom_density(alpha = 0.5) +
    theme_bw() + xlab("Estimate") +
    ylab("Density") + theme(
      legend.position = "bottom",
      legend.box.background = element_rect(),
    ) +
    facet_grid(rows = "measure", scales = "free")
  
  if ("ground_truth" %in% names(res)) {
    p <- p + geom_vline(aes(xintercept = ground_truth), color = "red", 
                        linetype = "dashed")
  }
  p
}

check_constraints <- function(x) {
  
  x <- as.data.table(x)
  x <- x[method == "faircause"]
  
  # constraint 1: TE = NDE + NIE
  c1 <- x[measure %in% c("TE", "NDE", "NIE"), c("value", "measure", "boot_num"),
          with = F]
  c1 <- dcast(c1, boot_num ~ measure)
  c1[, TE_tot := NDE / TE + NIE / TE]
  
  # constraint 2: TV = TE + ExpSE_x1 - ExpSE_x0
  c2 <- x[measure %in% c("TV", "TE", "ExpSE_x1", "ExpSE_x0"), 
          c("value", "measure", "boot_num"),
          with = F]
  c2 <- dcast(c2, boot_num ~ measure)
  c2[, TV_tot := TE/TV + ExpSE_x1/TV - ExpSE_x0/TV]
  
  # constraint 3: ETT = DE + IE
  c3 <- x[measure %in% c("ETT", "DE", "IE"), c("value", "measure", "boot_num"),
          with = F]
  c3 <- dcast(c3, boot_num ~ measure)
  c3[, ETT_tot := DE / ETT + IE / ETT]
  
  # constraint 4: TV = ETT - Ctf-SE
  c4 <- x[measure %in% c("TV", "ETT", "SE"), c("value", "measure", "boot_num"),
          with = F]
  c4 <- dcast(c4, boot_num ~ measure)
  c4[, TV2_tot := ETT / TV - SE / TV]
  
  dat <- cbind(c1$TE_tot, c2$TV_tot, c3$ETT_tot, c4$TV2_tot, c1$boot_num)
  dat <- data.table(dat)
  names(dat) <- c(
    paste("Normalized", 
          c("TE = NDE + NIE", "TV = TE + ExpSE_x1 - ExpSE_x0",
            "ETT = Ctf-DE + Ctf-IE", "TV = ETT - Ctf-SE")),
    "boot_num"
  )
  dat <- melt(dat, id.vars = "boot_num")
  dat$value <- dat$value + rnorm(length(dat$value), sd = 0.01)
  
  ggplot(dat, aes(x = value, fill = factor(variable))) +
    geom_density() +
    theme_bw() +
    scale_fill_discrete(name = "Constraint") +
    theme(
      legend.position = c(0.8, 0.8),
      legend.box.background = element_rect()
    )
  
}

method_cmp <- function(measure = c("DE", "ETT", "ETU", "ExpSE_x0", "ExpSE_x1", 
                                   "IE", "NDE", "NIE", "SE", "TE", "TV"), 
                       example = "med", nboot, nsamp, model) {
  
  if (example == "nomed") {
    gt <- ex_nomed(10^5, "gtruth")
  } else if (example == "med") {
    gt <- ex_med(10^5, "gtruth")
  } else gt <- NULL

  est <- NULL
  for(i in seq_len(nboot)) {
    
    # get the example
    ex <- get_ex(example, nsamp = nsamp, boot_num = i)
    
    # apply the cookbook
    cbook <- fairness_cookbook(ex$dat, ex$X, ex$W, ex$Z, ex$Y, 0, 1,
                               model = model)
    cb <- data.frame(measure = names(cbook$measures),
                     value = vapply(cbook$measures, \(x) x[1], numeric(1L)),
                     method = "faircause")

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
  
  crf <- causal_forest(
    X = ex$dat[, ex$Z],
    Y = ex$dat[[ex$Y]],
    W = ex$dat[[ex$X]]
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
  
  cwg <- medDML(
    y = ex$dat[[ex$Y]],
    d = ex$dat[[ex$X]],
    m = ex$dat[, ex$W],
    x = ex$dat[, ex$Z]
  )
  
  data.frame(
    measure = c("NDE", "NIE"), 
    value = c(cwg$results["effect", "dir.treat"], 
              cwg$results["effect", "indir.treat"]),
    method = "causalweight"
  )
}

dre <- function(ex) {
  
  dat0 <- dat1 <- dat <- ex$dat
  dat0$X <- 0
  dat1$X <- 1
  n <- nrow(dat)
  
  mu1 <- model_mean(as.formula(paste0("Y ~ X + ", paste0(ex$Z, collapse = "+"))), 
                    dat, dat1, probability = length(unique(ex$dat[[ex$Y]])) == 2L)
  mu0 <- model_mean(as.formula(paste0("Y ~ X + ", paste0(ex$Z, collapse = "+"))), 
                    dat, dat0, probability = length(unique(ex$dat[[ex$Y]])) == 2L)
  
  prop1 <- model_propensity(
    as.formula(paste0("X ~ ", paste0(ex$Z, collapse = "+"))), ex$dat, xlvl = 1)
  
  idx <- ex$dat[[ex$X]] == 1
  dr_mu1 <- 1 / n * sum( (ex$dat[[ex$Y]] - mu1)[idx] / prop1[idx]  ) + mean(mu1)
  dr_mu0 <- 1 / n * sum((ex$dat[[ex$Y]] - mu0)[!idx] / (1 - prop1[!idx])) + mean(mu0) 
  DR_ATE <- dr_mu1 - dr_mu0
  
  data.frame(measure = "TE", value = DR_ATE, method = "DR")
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
    
  }
  
  list(
    dat = dat,
    Z = grep("^Z", names(dat), value = TRUE),
    W = grep("^W", names(dat), value = TRUE),
    X = "X", Y = "Y"
  )
  
}
