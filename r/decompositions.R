
c_eff <- function(form, data, int.data, ...) {

  rf <- ranger(form, data = data, keep.inbag = T, importance = "impurity", ...)
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

fairness_cookbook <- function(data, X, W, Z, Y, x0, x1, 
                              nboot = 100, model = "ranger", ...) {
  
  idx <- data[[X]] == x0
  int.data <- data
  int.data[[X]] <- factor(x1, levels = levels(data[[X]]))
  int.data2 <- int.data
  int.data2[[X]] <- factor(x0, levels = levels(data[[X]]))
  
  y <- as.numeric(data[[Y]]) - is.factor(data[[Y]]) # need to check (!)
  
  # total
  #form <- as.formula(paste(Y, "~", paste(c(X, Z), collapse = "+")))
  #yx0 <- c_eff(form, data, int.data2, ...)
  #yx1 <- c_eff(form, data, int.data, ...)
  
  # nested
  #form <- as.formula(paste(Y, "~", paste(c(X, W, Z), collapse = "+")))
  #yx1wx0 <- c_eff(form, data, int.data, ...)
  
  #form <- as.formula(paste(Y, "~", paste(c(X, W, Z), collapse = "+")))
  #yx0wx1 <- c_eff(form, data, int.data, ...)
  
  est <- doubly_robust(data[[X]], data[, Z], data[, W], data[[Y]],
                       model = model)
  yx0 <- est[[1]]
  yx1 <- est[[2]]
  yx1wx0 <- est[[4]]
  
  # bootstrap subsamples
  boots <- lapply(
    seq_len(nboot),
    function(i) {
      
      ind <- sample.int(nrow(data), replace = TRUE)
      is0 <- data[[X]][ind] == x0
      ind0 <- ind[is0]
      ind1 <- ind[!is0]
      
      list(all = ind, id0 = ind0, id1 = ind1)
      
    }
  )
  
  msd <- function(x1, t1, x2, t2) {
    
    ms <- vapply(
      boots, 
      function(ids) {
        mean(x1[ids[[t1]]], na.rm = TRUE) -  mean(x2[ids[[t2]]], na.rm = TRUE)
      }, numeric(1L)
    )
    
    c(mean(ms), sd(ms))
    
  }
   
  # get TV
  tv <- msd(y, "id1", y, "id0")

  # get DE
  ctfde <- msd(yx1wx0, "id0", yx0, "id0") # updated (!)
  nde <- msd(yx1wx0, "all", yx0, "all") # updated (!)

  # get SE

  se <- msd(yx1, "id0", y, "id1") # pyx1_x0 - py_x1
  expse_x1 <- msd(yx1, "all", y, "id1") # pyx1 - py_x1
  expse_x0 <- msd(yx0, "all", y, "id0") # pyx0 - py_x0

  # get ETT
  ett <- msd(yx1, "id0", y, "id0") # pyx1_x0 - py_x0
  te <- msd(yx1, "all", yx0, "all") # pyx1 - pyx0
  
  nie <- msd(yx1, "all", yx1wx0, "all")
  ctfie <- msd(yx1, "id0", yx1wx0, "id0")

  structure(
    list(measures = list(
      TV = tv, DE = ctfde, SE = se, ETT = ett, IE = ctfie,
      TE = te, NDE = nde, NIE = nie, ExpSE_x1 = expse_x1,
      ExpSE_x0 = expse_x0
    ), x0 = x0, x1 = x0, model = model, X = X, W = W, Z = Z, Y = Y, 
    cl = match.call()),
    class = "faircause"
  )

}

CausalExplanation_EO <- function(data, X, W, Z, Y, x0, x1, ylvl, ...) {

  idx <- data[[X]] == x0
  int.data <- data
  int.data[[X]] <- factor(x1, levels = levels(data[[X]]))

  y_idx <- data[[Y]] == ylvl

  form <- as.formula(paste(
    Y, "~", X, "+", paste(W, collapse = "+"),
    "+", paste(Z, collapse = "+")
  ))

  y_hat <- ranger(form, data = data, classification = T)$predictions

  # get EO
  eo <- mean(y_hat[!idx & y_idx]) - mean(y_hat[idx & y_idx])


  # get DE

  pyhat_x1x0_dir <- c_eff(form, data, int.data, idx & y_idx, ...)
  de <- pyhat_x1x0_dir - c_eff(form, data, data, idx & y_idx, ...)

  # get SE
  form <- as.formula(paste(
    Y, "~", X, "+", paste(Z, collapse = "+")
  ))

  se <- pyhat_x1x0_dir - c_eff(form, data, int.data, !idx & y_idx, ...)

  ie <- eo - de + se

  list(er = eo, ers = se, erd = de, eri = ie)

}

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

TV_family <- function(tvd, dataset) {
  
}

