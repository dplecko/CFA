#' fair_decisions
#'
#' Implementation of the Outcome Control algorithm described in Causal Fairness
#' Analysis (Plecko & Bareinboim 2024). Uses only plain \code{R}.
#'
#' The procedure takes the data as an input, together with
#' the causal graph given by the Standard Fairness Model, and a choice of a
#' specific control/decision variable (input `D`). It outputs an S3 class object
#' of type `fair_decision` which contains the estimates of the conditional
#' average treatment effects (CATEs) of variable \eqn{D} on \eqn{Y}, conditional
#' on the values of \eqn{X, Z, W}. This quantity is also referred to as the
#' benefit, and is defined as:
#' \deqn{\Delta := E[Y_{d_1} - Y_{d_0} \mid x, z, w].}
#' Subsequently, several steps of Outcome Control are performed. First, the
#' causal decomposition of the original decision \eqn{D} is performed. Secondly,
#' the decomposition of the benefit \eqn{\Delta} is performed. Finally, the
#' benefit fairness criterion is investigated. All of these can be inspected by
#' applying the `autoplot()` function on the S3 object. Also, the object allows
#' for predictions of the benefit values, and of construct decisions, using the
#' `predict()` function.
#'
#' @inheritParams fairness_cookbook
#' @param D `character(1L)` with the name of the decision/control variable.
#' @param xgb_params `xgboost` parameters passed to the fit.
#' @param xgb_nrounds Number of boosting rounds in `xgboost`.
#' @param po_transform An arbitrary transformation function that can be
#' applied to the potential outcomes \eqn{Y_{d}} when computing the benefit
#' \eqn{\Delta}, meaning that \eqn{E[f(Y_{d_1}) - f(Y_{d_0}) \mid x, z, w]} is
#' used for \eqn{\Delta}.
#' @param po_diff_sign The difference of potential outcomes
#' \eqn{Y_{d_1} - Y_{d_0}} sometimes may have a specific sign (-1 or 1). When
#' this argument is specified, the sign is enforced.
#' Default value is 0, which (in principle) allows for both
#' positive and negative differences (i.e., there is no monotonicity).
#' @param ... Further arguments passed to downstream model fitting functions.
#'
#' @return An object of class \code{fair_decision} with elements:
#'   \item{\code{d_fcb}, \code{delta_fcb}}{Fairness Cookbook objects for the
#'   decision \eqn{D} and the benefit \eqn{\Delta}.}
#'   \item{\code{delta}}{Estimated values of the benefit \eqn{\Delta}.}
#'   \item{\code{data}, \code{po_diff_sign}, \code{po_transform},
#'   \code{xgb_params}}{See input definitions.}
#'   \item{\code{xgb_mod}}{\code{xgboost} model for estimating the benefit
#'   \eqn{\Delta}.}
#'   \item{\code{X, Z, W, Y, D}}{Names of protected attribute, confounders,
#'   mediators, outcome, and the decision/control variable.}
#'   \item{\code{x0, x1}}{Levels of protected attribute.}
#'   \item{\code{method}}{Estimation method. See `@param method`.}
#'   \item{\code{model}}{Model class for \code{method == "medDML"}.}
#'   \item{cl}{Generating function call.}
#'   \item{params}{If \code{tune_params == TRUE} in the function call, this object
#' is a list of optimal \code{min.node.size} values for each tree-based used
#' in the estimation procedure. See `faircause:::doubly_robust_med()` and
#' `faircause::crf_wrap()` for more details about the used objects.}
#'
#' @author Drago Plecko
#' @references
#' Plecko, D. & Bareinboim, E. (2022).
#' Causal Fairness Analysis \cr
#' @import stats
#' @importFrom assertthat assert_that
#' @importFrom grf causal_forest
#' @importFrom reticulate py_run_file source_python
#' @export
fair_decisions <- function(data, X, Z, W, Y, D, x0, x1,
                           xgb_params = list(eta = 0.1), xgb_nrounds = 100,
                           po_transform = function(x) x, po_diff_sign = 0,
                           method = c("medDML", "causal_forest"),
                           model = c("ranger", "linear"), tune_params = FALSE,
                           nboot1 = 1L, nboot2 = 100L, ...) {

  verify_numeric_input(data)

  method <- match.arg(method, c("medDML", "causal_forest"))
  model <- match.arg(model, c("ranger", "linear"))

  # decompose the disparity in resource allocation
  d_fcb <- fairness_cookbook(data, X = X, Z = Z, W = W, Y = D, x0 = x0, x1 = x1,
                             model = model, method = method,
                             tune_params = tune_params,
                             nboot1 = 1L, nboot2 = 100L, ...)


  # split train and eval ?
  # train_idx <- seq_len(round((1 - eval_prop) * nrow(data)))
  # train_data <- data[train_idx, ]
  # eval_data <- data[-train_idx, ]

  # estimate the delta parameter
  xgbcv <- xgboost::xgb.cv(
    params = xgb_params,
    data = as.matrix(data[, c(X, Z, W, D)]),
    label = data[[Y]], nrounds = xgb_nrounds,
    early_stopping_rounds = 5, nfold = 10, verbose = FALSE
  )

  # pick optimal number of rounds
  xgb_nrounds <- xgbcv$best_iteration

  # train the prediction object
  xgb_mod <- xgboost::xgboost(
    params = xgb_params, data = as.matrix(data[, c(X, Z, W, D)]),
    label = data[[Y]], nrounds = xgb_nrounds, verbose = FALSE
  )

  data$delta <- predict_delta(xgb_mod, data, X, Z, W, D, po_diff_sign,
                              po_transform)

  # perform a decomposition on Delta
  delta_fcb <- fairness_cookbook(data, X = X, Z = Z, W = W, Y = D,
                                 x0 = x0, x1 = x1,
                                 model = model, method = method,
                                 tune_params = tune_params,
                                 nboot1 = 1L, nboot2 = 100L, ...)

  # output a faircause object
  structure(
    list(
      d_fcb = d_fcb,
      delta_fcb = delta_fcb,
      data = data, delta = data$delta,
      po_diff_sign = po_diff_sign, po_transform = po_transform,
      xgb_mod = xgb_mod, xgb_params = xgb_params,
      x0 = x0, x1 = x1, model = model, X = X, W = W, Z = Z, Y = Y, D = D,
      cl = match.call(),
      method = method,
      tune_params = tune_params, nboot1 = nboot1, nboot2 = nboot2
    ),
    class = "fair_decision"
  )
}
