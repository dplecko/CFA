#' fair_predictions
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
      reticulate::py$pred_nn_proba(nn_mod[[i]], eval_data[ , c(X, Z, W)])
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
      y_meas = y_meas,
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
