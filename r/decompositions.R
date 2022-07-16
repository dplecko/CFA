#' fairness_cookbook
#'
#' Implementation of Fairness Cookbook in Causal Fairness Analysis
#' (Plecko & Bareinboim 2022). Uses only plain \code{R}.
#'
#' The procedure takes the training and testing data as an input, together with
#' the causal graph given by an adjacency matrix and the list of resolving
#' variables, which should be kept fixed during the adaptation procedure. The
#' procedure then calculates a fair representation of the data, after which
#' any classification method can be used. There are, however, several valid
#' training options yielding fair predictions, and the best of them can be
#' chosen with cross-validation. For more details we refer the user to the
#' original paper. Most of the running time is due to the quantile regression
#' step using the ranger package.
#'
#' @param data Object of class \code{data.frame} containing the dataset.
#' @param X A \code{character} scalar giving the name of the
#' protected attribute. Must be one of the entries of \code{names(data)}.
#' @param W A \code{character} vector giving the names of all mediators.
#' @param Z A \code{character} vector giving the names of all confounders.
#' @param Y A \code{character} scalar giving the name of the outcome.
#' @param x0,x1 Scalar values giving the two levels of the binary protected
#' attribute.
#' @param nboot An \code{integer} scalar determining the number of bootstrap
#' repetitions used when constructing confidence intervals of the estimates.
#' @param model A \code{character} scalar determining the model used in
#' estimation. Current options are \code{"ranger"} (default) which uses random
#' forests and \code{"linear"} which uses (generalized) linear models.
#' @param ... Further arguments passed to downstream model fitting functions.
#'
#' @return An object of class \code{faircause}, containing estimates of the
#' causal fairness measures, together with some meta information.
#'
#' @examples
#' \dontrun{
#' data <- faircause:::get_data_berkeley()
#'
#' FCB <- fairness_cookbook(data, X = "gender", W = "dept", Z = character(0L),
#'                          Y = "admit", x0 = "Male", x1 = "Female")
#' FCB
#' }
#'
#'
#' @author Drago Plecko
#' @references
#' Plecko, D. & Bareinboim, E. (2022).
#' Causal Fairness Analysis \cr
#' @import stats
#' @importFrom assertthat assert_that
#' @export
fairness_cookbook <- function(data, X, W, Z, Y, x0, x1,
                              nboot = 100, model = "ranger", ...) {

  y <- as.numeric(data[[Y]]) - is.factor(data[[Y]]) # need to check (!)

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
  # mean-sd helper
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

  ### non-mediated estimates
  {

    data0 <- data1 <- data
    if (!is.factor(data[[X]])) {
      data[[X]] <- factor(data[[X]], levels = c(x0, x1))
    }
    data0[[X]] <- factor(x0, levels = c(x0, x1))
    data1[[X]] <- factor(x1, levels = c(x0, x1))

    form <- as.formula(paste(Y, "~", paste(c(X, Z), collapse = "+")))

    mux1 <- model_mean(form, data, data1, Y = Y,
                       probability = length(unique(data[[Y]])) == 2L)

    mux0 <- model_mean(form, data, data0, Y = Y,
                       probability = length(unique(data[[Y]])) == 2L)

    idx <- data[[X]] == x1

    if (length(Z) > 0) {

      propx1 <- model_propensity(
        as.formula(paste0(X, " ~ ", paste0(Z, collapse = "+"))), data, Y = X,
        xlvl = x1)
    } else {

      propx1 <- rep(mean(idx), nrow(data))
    }


    yx1 <- (data[[Y]] - mux1) * idx / propx1 + mux1
    yx0 <- (data[[Y]] - mux0) * (!idx) / (1 - propx1) + mux0

    extrm_idx <- propx1 < 0.01 | propx1 > 0.99
    yx1[extrm_idx] <- yx0[extrm_idx] <- NA
    if (mean(extrm_idx) > 0.02) {
      message(100 * mean(extrm_idx),
              "% of extreme P(x | z) probabilities.\n",
              "TE, ETT, Exp-SE, Ctf-SE estimates likely biased.")
    }

  }
  # get SE
  ctfse <- msd(yx1, "id0", y, "id1") # Ctf-SE_{x_1, x_0}(y) = pyx1_x0 - py_x1
  expse_x1 <- msd(y, "id1", yx1, "all") # py_x1 - pyx1
  expse_x0 <- msd(y, "id0", yx0, "all") # py_x0 - pyx0
  # get TE/ETT
  ett <- msd(yx1, "id0", y, "id0") # pyx1_x0 - py_x0
  te <- msd(yx1, "all", yx0, "all") # pyx1 - pyx0

  ### mediated
  {

    est_med <- doubly_robust_med(data[[X]], data[, Z], data[, W], data[[Y]],
                                 model = model)
    yx0 <- est_med[[1]]
    yx1 <- est_med[[2]]
    yx1wx0 <- est_med[[4]]
  }
  # get DE
  nde <- msd(yx1wx0, "all", yx0, "all") # NDE_{x_0, x_1}(y)
  ctfde <- msd(yx1wx0, "id0", y, "id0") # Ctf-DE_{x_0, x_1}(y | x_0)
  # get IE
  nie <- msd(yx1wx0, "all", yx1, "all") # NIE_{x_1, x_0}(y)
  ctfie <- msd(yx1wx0, "id0", yx1, "id0") # Ctf-IE_{x_1, x_0}(y | x_0)

  # output
  structure(
    list(measures = list(
      TV = tv, CtfDE = ctfde, CtfSE = ctfse, ETT = ett, CtfIE = ctfie,
      TE = te, NDE = nde, NIE = nie, ExpSE_x1 = expse_x1,
      ExpSE_x0 = expse_x0
    ), x0 = x0, x1 = x0, model = model, X = X, W = W, Z = Z, Y = Y,
    cl = match.call()),
    class = "faircause"
  )

}

# CausalExplanation_EO <- function(data, X, W, Z, Y, x0, x1, ylvl, ...) {
#
#   idx <- data[[X]] == x0
#   int.data <- data
#   int.data[[X]] <- factor(x1, levels = levels(data[[X]]))
#
#   y_idx <- data[[Y]] == ylvl
#
#   form <- as.formula(paste(
#     Y, "~", X, "+", paste(W, collapse = "+"),
#     "+", paste(Z, collapse = "+")
#   ))
#
#   y_hat <- ranger(form, data = data, classification = T)$predictions
#
#   # get EO
#   eo <- mean(y_hat[!idx & y_idx]) - mean(y_hat[idx & y_idx])
#
#
#   # get DE
#
#   pyhat_x1x0_dir <- c_eff(form, data, int.data, idx & y_idx, ...)
#   de <- pyhat_x1x0_dir - c_eff(form, data, data, idx & y_idx, ...)
#
#   # get SE
#   form <- as.formula(paste(
#     Y, "~", X, "+", paste(Z, collapse = "+")
#   ))
#
#   se <- pyhat_x1x0_dir - c_eff(form, data, int.data, !idx & y_idx, ...)
#
#   ie <- eo - de + se
#
#   list(er = eo, ers = se, erd = de, eri = ie)
#
# }

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
