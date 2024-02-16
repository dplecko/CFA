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
#' @param Z A \code{character} vector giving the names of all mediators.
#' @param W A \code{character} vector giving the names of all confounders.
#' @param Y A \code{character} scalar giving the name of the outcome.
#' @param x0,x1 Scalar values giving the two levels of the binary protected
#' attribute.
#' @param eo A \code{logical(1L)} indicating whether the object should be of
#' type equality of odds or total variation. Default is \code{FALSE}.
#' @param method A \code{character} scalar with two options: \code{"medDML"} for
#' mediation double-machine learning, \code{"causal_forest"} for the
#' [grf::causal_forest()] method from the \code{grf} package.
#' @param model A \code{character} scalar taking values in
#' \code{c("ranger", "linear")}, indicating whether a tree-based learner is used
#' [ranger::ranger()], or if the fitted model should be linear. This parameter
#' is only relevant if \code{method == "medDML"}.
#' @param tune_params A \code{logical(1L)} indicating whether the parameters
#' should be tuned for the tree-based methods (only the \code{min.node.size}
#' parameter is tuned). Defaults to \code{FALSE}.
#' @param nboot1 An \code{integer} scalar determining the number of outer
#' bootstrap repetitions, that is, how many times the fitting procedure is
#' repeated. Default is \code{1L}.
#' @param nboot2 An \code{integer} scalar determining the number of inner
#' bootstrap repetitions, that is, how many bootstrap samples are taken after
#' the potential outcomes are obtained from the estimation procedure.
#' Default is \code{100L}.
#' @param ... Further arguments passed to downstream model fitting functions.
#'
#' @return An object of class \code{faircause}, containing the following
#' elements:
#' \item{\code{measures}}{A \code{data.frame} containing the estimates for each
#' combination of measure/outer bootstrap repetition/inner bootstrap repetition.}
#' \item{\code{X, Z, W, Y}}{Names of the protected attribute, confounders,
#' mediators, and the outcome, respectively.}
#' \item{\code{x0, x1}}{Protected attribute levels.}
#' \item{\code{method}}{Method of estimation (see parameters above).}
#' \item{\code{model}}{Model class of the fit (relevant if
#' \code{method == "medDML"}, see parameters above).}
#' \item{cl}{The function call that generated the object.}
#' \item{eo}{Logical indicator whether the object is an equality of odds object.}
#' \item{params}{If \code{tune_params == TRUE} in the function call, this object
#' is a list of optimal \code{min.node.size} values for each tree-based used
#' in the estimation procedure. See `faircause:::doubly_robust_med()` and
#' `faircause::crf_wrap()` for more details about the used objects.}
#' @examples
#' \dontrun{
#' data <- faircause::berkeley
#'
#' fcb <- fairness_cookbook(data, X = "gender", Z = character(0L), W = "dept",
#'                          Y = "admit", x0 = "Male", x1 = "Female")
#' fcb
#' }
#'
#'
#' @author Drago Plecko
#' @references
#' Plecko, D. & Bareinboim, E. (2022).
#' Causal Fairness Analysis \cr
#' @import stats
#' @importFrom assertthat assert_that
#' @importFrom grf causal_forest
#' @export
fairness_cookbook <- function(data, X, Z, W, Y, x0, x1, eo = FALSE,
                              method = c("medDML", "causal_forest"),
                              model = c("ranger", "linear"), tune_params = FALSE,
                              nboot1 = 1L, nboot2 = 100L, ...) {

  method <- match.arg(method, c("medDML", "causal_forest"))
  model <- match.arg(model, c("ranger", "linear"))

  y <- as.numeric(data[[Y]]) - is.factor(data[[Y]]) # need to check (!)

  # handle empty Z/W sets
  Z <- if (length(Z) == 0 | identical(Z, "")) NULL else Z
  W <- if (length(W) == 0 | identical(W, "")) NULL else W

  # coerce X to a factor with levels x0, x1
  data[[X]] <- factor(data[[X]], levels = c(x0, x1))
  idx <- data[[X]] == x1

  params <- res <- list()

  if (method == "causal_forest") {

    for (rep in seq_len(nboot1)) {

      res[[rep]] <- ci_crf(data, X, Z, W, Y, x0, x1, rep, nboot = nboot2,
                           tune_params = tune_params, params = params)
      if (length(res) < rep) {

        method <- "medDML"
        break
      }
      params <- attr(res[[rep]], "params")
    }
    res <- do.call(rbind, res)
  }

  if (method == "medDML") {

    for (rep in seq_len(nboot1)) {

      res[[rep]] <- ci_mdml(data, X, Z, W, Y, x0, x1, model, rep,
                            nboot = nboot2, tune_params = tune_params,
                            params = params)
      params <- attr(res[[rep]], "params")
    }
    res <- do.call(rbind, res)
  }

  # output a faircause object
  structure(
    list(
      measures = res,
      x0 = x0, x1 = x1, model = model, X = X, W = W, Z = Z, Y = Y,
      cl = match.call(),
      eo = eo,
      method = method,
      params = params),
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
fairness_cookbook_eo <- function(data, X, Z, W, Y, Yhat, x0, x1, ylvl,
                                 method = c("medDML", "causal_forest"),
                                 model = c("ranger", "linear"),
                                 tune_params = FALSE,
                                 nboot1 = 1L, nboot2 = 100L, ...) {

  y_idx <- data[[Y]] == ylvl
  fairness_cookbook(data = data[y_idx, ], X = X, Z = Z, W = W, Y = Yhat,
                    x0 = x0, x1 = x1, eo = TRUE)
}
