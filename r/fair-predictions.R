#' fair_predictions
#'
#' Implementation of the Fair Prediction algorithm described in Causal Fairness
#' Analysis (Plecko & Bareinboim 2024). Uses only plain \code{R}.
#'
#' The procedure takes the data as an input, together with
#' the causal graph given by the Standard Fairness Model, and the choice of
#' the business necessity set. It outputs an S3 class object of type
#' `fair_prediction` which contains model fits that satisfy the desired causal
#' constraints. The optimized loss function is the following:
#' \deqn{L(y, \hat{y}) + \lambda \Big( | \text{NDE}_{x_0, x_1}(\hat{y}) - \eta_1 |
#' + | \text{NIE}_{x_1, x_0}(\hat{y}) - \eta_2 | + | \text{Exp-SE}_{x_1}(\hat{y})
#'  - \eta_3 | + | \text{Exp-SE}_{x_0}(\hat{y}) - \eta_4 | \Big).}
#' \eqn{L(y, \hat{y})} is either cross-entropy (for classification) or mean
#' squared error (for regression). The values of \eqn{\eta_1, \eta_2, \eta_3,
#' \eta_4} are either 0 if the effect is not in the business necessity set, or
#' equal the effect estimate for the true outcome \eqn{Y} (if the effect is BN).
#' The fitting process is repeated over a range of \eqn{\lambda} values, which
#' can be controlled using the `lmbd_seq` argument.
#' The quality of the fit can be assessed by applying `autoplot()` to the S3
#' object. Predictions on test data can be obtained by applying `predict()`.
#'
#' @inheritParams fairness_cookbook
#' @param BN A character vector with any combination of entries
#' `"DE"`, `"IE"`, `"SE"` representing the business necessity set.
#' @param eval_prop Proportion of the data that should be used as a held-out
#' evaluation set for the purposes of early stopping in the neural network fit.
#' @param lr Learning rate of the fitting process (for Adam optimizer).
#' @param lmbd_seq A sequence of values of \eqn{\lambda}, the parameter value
#' which balances between the standard loss functions (cross-entropy for
#' classification; mean squared error for regression) and the causal loss, which
#' measures how closely satisfied the desired causal constraints are. See more
#' in the details.
#' @param relu_eps A special type of penalization that allows small deviations
#' from the causal constraints. Default is `FALSE`. When changing to `TRUE`,
#' a different value of the \eqn{\lambda} may be more appropriate.
#' @param patience Number of epochs of patience before triggering early
#' stopping.
#' @param ... Further arguments passed to downstream model fitting functions.
#'
#' @return An object of class \code{fair_prediction} with elements:
#'   \item{\code{yhat_meas}, \code{y_meas}}{Causal fairness summaries for
#'   \eqn{\widehat{Y}} and \eqn{Y}.}
#'   \item{\code{task_type}}{Fit type: regression/classification.}
#'   \item{\code{train_data}, \code{eval_data}}{Training and evaluation data.}
#'   \item{\code{lr}, \code{patience}, \code{BN}}{See input definitions.}
#'   \item{\code{neural_models}}{A list of neural network models obtained during
#'   the model fitting. Note that these are python objects.}
#'   \item{\code{X, Z, W, Y}}{Names of protected attribute, confounders,
#'   mediators, outcome.}
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
fair_predictions <- function(data, X, Z, W, Y, x0, x1, BN = "",
                             eval_prop = 0.25, lr = 0.001,
                             lmbd_seq = c(0.1, 0.5, 1, 2, 5, 10),
                             relu_eps = FALSE, patience = 100,
                             method = c("medDML", "causal_forest"),
                             model = c("ranger", "linear"), tune_params = FALSE,
                             nboot1 = 1L, nboot2 = 100L, ...) {

  verify_numeric_input(data)
  method <- match.arg(method, c("medDML", "causal_forest"))
  model <- match.arg(model, c("ranger", "linear"))

  # decompose the true outcome
  y_fcb <- fairness_cookbook(data, X = X, Z = Z, W = W, Y = Y, x0 = x0, x1 = x1,
                             model = model, method = method,
                             tune_params = tune_params,
                             nboot1 = 1L, nboot2 = 100L, ...)

  train_idx <- seq_len(round((1 - eval_prop) * nrow(data)))
  train_data <- data[train_idx, ]
  eval_data <- data[-train_idx, ]

  # extract the ground truth for true outcome
  y_meas <- summary(y_fcb)$measures

  # list of neural models
  nn_mod <- replicate(length(lmbd_seq), NULL)
  names(nn_mod) <- paste("Lambda =", lmbd_seq)

  # check if py deps are available
  if (!reticulate::py_module_available("torch")) {
    stop(paste(
      "'torch' not available to reticulate but is required for this feature.",
      "Please install 'torch' and make it available to reticulate."
    ))
  }

  # load python dependencies
  load_py_deps()

  # get task_type (regression/classification)
  task_type <- ifelse(length(unique(data[[Y]])) > 2, "regression",
                      "classification")

  # fit models across lambda values
  res <- NULL
  for (i in seq_along(lmbd_seq)) {

    lmbd <- lmbd_seq[i]

    # fit a neural network for a fixed lambda
    nn_mod[[i]] <- reticulate::py$train_w_es(
      train_data, eval_data, x_col = X, w_cols = W, z_cols = Z,
      y_col = Y, lmbd = lmbd, lr = lr,
      nde = !is.element("DE", BN),
      nie = !is.element("IE", BN),
      nse = !is.element("SE", BN),
      eta_de = y_meas[y_meas$measure == "nde",]$value,
      eta_ie = y_meas[y_meas$measure == "nie",]$value,
      eta_se_x1 = y_meas[y_meas$measure == "expse_x1",]$value,
      eta_se_x0 = y_meas[y_meas$measure == "expse_x0",]$value,
      relu_eps = relu_eps, verbose = FALSE, patience = patience
    )

    # get the predictions on the
    eval_data$preds <- as.vector(
      reticulate::py$pred_nn_proba(nn_mod[[i]], eval_data[ , c(X, Z, W)],
                                   task_type)
    )
    eval_fcb <- fairness_cookbook(eval_data, X = X, Z = Z, W = W,
                                  Y = "preds", x0 = x0, x1 = x1,
                                  model = model, method = method,
                                  tune_params = tune_params,
                                  nboot1 = 1L, nboot2 = 100L, ...)

    y_eval <- eval_data[[Y]]
    p_eval <- eval_data[["preds"]]
    meas <- summary(eval_fcb)$measures
    res <- rbind(
      res, lambda_performance(meas, y_eval, p_eval, lmbd)
    )
  }

  # output a faircause object
  structure(
    list(
      yhat_meas = res,
      y_meas = y_meas, task_type = task_type,
      BN = BN, train_data = train_data, eval_data = eval_data,
      lmbd_seq = lmbd_seq, lr = lr, patience = patience, relu_eps = relu_eps,
      neural_models = nn_mod,
      x0 = x0, x1 = x1, model = model, X = X, W = W, Z = Z, Y = Y,
      cl = match.call(),
      method = method,
      tune_params = tune_params, nboot1 = nboot1, nboot2 = nboot2
    ),
    class = "fair_prediction"
  )
}
