#' fairness_cookbook
#'
#' Implementation of Fairness Cookbook in Causal Fairness Analysis
#' (Plecko & Bareinboim 2022). Uses only plain \code{R}.
#'
#' The procedure takes the data as an input, together with
#' the causal graph given by the Standard Fairness Model, and outputs a causal
#' decomposition of the TV measure into direct, indirect, and spurious effects.
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
#' @param eo A \code{logical(1L)} indicating whether the object is of type
#' equality of odds or total variation. Default is false.
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
fairness_cookbook <- function(data, X, W, Z, Y, x0, x1, eo = FALSE,
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
    } else {
      data[[X]] <- relevel(data[[X]], ref = which(levels(data[[X]]) == x0))
    }
    data0[[X]] <- factor(x0, levels = c(x0, x1))
    data1[[X]] <- factor(x1, levels = c(x0, x1))

    form <- as.formula(paste(Y, "~", paste(c(X, Z), collapse = "+")))

    mux1 <- model_mean(form, data, data1, Y = Y,
                       probability = length(unique(data[[Y]])) == 2L, ...)

    mux0 <- model_mean(form, data, data0, Y = Y,
                       probability = length(unique(data[[Y]])) == 2L, ...)

    idx <- data[[X]] == x1

    if (length(Z) > 0) {

      propx1 <- model_propensity(
        as.formula(paste0(X, " ~ ", paste0(Z, collapse = "+"))), data, Y = X,
        xlvl = which(levels(data[[X]]) == x1), ...)
    } else {

      propx1 <- rep(mean(idx), nrow(data))
    }

    yx1 <- (y - mux1) * idx / propx1 + mux1
    yx0 <- (y - mux0) * (!idx) / (1 - propx1) + mux0

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
  if (length(W) > 0) {

    est_med <- doubly_robust_med(data[[X]], data[, Z], data[, W], data[[Y]],
                                 model = model)
    yx0 <- est_med[[1]]
    yx1 <- est_med[[2]]
    yx1wx0 <- est_med[[4]]

    # get DE
    nde <- msd(yx1wx0, "all", yx0, "all") # NDE_{x_0, x_1}(y)
    ctfde <- msd(yx1wx0, "id0", y, "id0") # Ctf-DE_{x_0, x_1}(y | x_0)
    # get IE
    nie <- msd(yx1wx0, "all", yx1, "all") # NIE_{x_1, x_0}(y)
    ctfie <- msd(yx1wx0, "id0", yx1, "id0") # Ctf-IE_{x_1, x_0}(y | x_0)
  } else {

    nde <- te
    ctfde <- ett
    ctfie <- nie <- c(0, 2 * 10^(-16))
  }


  # output
  structure(
    list(measures = list(
      TV = tv, CtfDE = ctfde, CtfSE = ctfse, ETT = ett, CtfIE = ctfie,
      TE = te, NDE = nde, NIE = nie, ExpSE_x1 = expse_x1,
      ExpSE_x0 = expse_x0
    ), x0 = x0, x1 = x1, model = model, X = X, W = W, Z = Z, Y = Y,
    cl = match.call(),
    eo = eo),
    class = "faircause"
  )

}

#' fairness_cookbook_eo
#'
#' Implementation of Fairness Cookbook in Causal Fairness Analysis
#' (Plecko & Bareinboim 2022), for the case when equality of odds is the
#' measure of interest.
#'
#' The procedure takes the data as an input, together with
#' the causal graph given by the Standard Fairness Model, and outputs a causal
#' decomposition of the EO measure into direct, indirect, and spurious effects.
#' @inheritParams fairness_cookbook
#' @param Yhat A \code{character} scalar giving the name of the constructed
#' predictor.
#' @param ylvl A value indicating within which group the decomposition should be
#' performed. For example, setting \code{ylvl = 1} would correspond to
#' performing causal effect decompositions for individuals who have
#' true outcome \code{Y == 1}.
#'
#' @export
fairness_cookbook_eo <- function(data, X, W, Z, Y, Yhat, x0, x1, ylvl, ...) {

  y_idx <- data[[Y]] == ylvl
  fairness_cookbook(data = data[y_idx, ], X = X, W = W, Z = Z, Y = Yhat,
                    eo = TRUE, x0 = x0, x1 = x1)
}
