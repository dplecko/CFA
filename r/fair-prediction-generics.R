
#' @importFrom ggplot2 autoplot ggplot aes geom_density ggtitle geom_col
#' @importFrom ggplot2 scale_fill_discrete xlab scale_y_continuous
#' @importFrom ggplot2 geom_errorbar geom_text theme_minimal position_fill theme
#' @importFrom ggplot2 element_text scale_x_discrete geom_hline
#' @importFrom latex2exp TeX
#' @export
autoplot.fair_prediction <- function(object, type = c("causal", "accuracy"),
                                     ...) {

  type <- match.arg(type, c("causal", "accuracy"))

  eval_meas <- object$yhat_meas
  y_meas <- object$y_meas
  y_meas <- cbind(y_meas, lmbd = -0.5)

  if (type == "causal") {

    ret <- lambda_solution_path(eval_meas, y_meas, object$BN)

  } else if (type == "accuracy") {

    if (object$task_type == "classification") {

      p_ent <- ggplot(eval_meas, aes(x = lmbd, y = bce)) +
        geom_point() + geom_line() + theme_minimal() +
        geom_ribbon(aes(ymin = bce - bce_sd, ymax = bce + bce_sd), alpha = 0.3) +
        ggtitle("Cross-entropy")

      p_acc <- ggplot(eval_meas, aes(x = lmbd, y = acc)) +
        geom_point() + geom_line() + theme_minimal() +
        geom_ribbon(aes(ymin = acc - acc_sd, ymax = acc + acc_sd), alpha = 0.3) +
        ggtitle("Accuracy")

      p_auc <- ggplot(eval_meas, aes(x = lmbd, y = auc)) +
        geom_point() + geom_line() + theme_minimal() +
        geom_ribbon(aes(ymin = auc - auc_sd, ymax = auc + auc_sd), alpha = 0.3) +
        ggtitle("AUC") + geom_hline(yintercept = 0.5, color = "orange")

      ret <- cowplot::plot_grid(p_ent, p_acc, p_auc, ncol = 3L)
    } else {

      ret <- ggplot(eval_meas, aes(x = lmbd, y = bce)) +
        geom_point() + geom_line() + theme_minimal() +
        geom_ribbon(aes(ymin = mse - mse_sd, ymax = mse + mse_sd), alpha = 0.3) +
        ggtitle("Mean Squared Error")
    }
  }

  ret
}


#' @export
predict.fair_prediction <- function(object, newdata, ...) {

  lmbd_seq <- object$lmbd_seq
  test_meas <- NULL
  preds <- replicate(length(lmbd_seq), NULL)
  names(preds) <- paste("Lambda =", lmbd_seq)
  y_meas <- object$y_meas
  y_meas <- cbind(y_meas, lmbd = -0.5)
  for (i in seq_along(lmbd_seq)) {

    lmbd <- lmbd_seq[i]

    # get the predictions on the
    newdata$preds <- preds[[i]] <- as.vector(
      reticulate::py$pred_nn_proba(object$neural_models[[i]],
                                   newdata[ , c(object$X, object$Z, object$W)],
                                   object$task_type)
    )
    test_fcb <- fairness_cookbook(newdata, X = object$X, Z = object$Z,
                                  W = object$W, Y = "preds",
                                  x0 = object$x0, x1 = object$x1,
                                  model = object$model, method = object$method,
                                  tune_params = object$tune_params,
                                  nboot1 = object$nboot1,
                                  nboot2 = object$nboot2, ...)

    y_test <- newdata[[Y]]
    p_test <- newdata[["preds"]]
    meas <- summary(test_fcb)$measures
    test_meas <- rbind(
      test_meas, lambda_performance(meas, y_test, p_test, lmbd)
    )
  }

  structure(
    list(
      predictions = preds,
      test_meas = test_meas, y_meas = y_meas,
      BN = object$BN
    ), class = "fair_prediction_test"
  )
}

#' @export
autoplot.fair_prediction_test <- function(object, ...) {

  lambda_solution_path(x$test_meas, object$y_meas, object$BN)
}

# helpers

#' @importFrom ggplot2 geom_hline
lambda_solution_path <- function(yhat_meas, y_meas, BN) {

  effect_path <- function(res, gt, meas, bn) {

    if (bn) {
      yintercept <- gt[gt$measure == meas,]$value
      clr <- "darkgreen"
    } else {
      yintercept <- 0
      clr <- "red"
    }

    ggplot(res[res$measure == meas,], aes(x = lmbd, y = value)) +
      geom_line() + geom_point() +
      geom_ribbon(aes(ymin = value - sd, ymax = value + sd), alpha = 0.3) +
      theme_minimal() +
      geom_point(data = gt[gt$measure == meas, ], aes(x = lmbd, y = value),
                 color = "blue") +
      geom_errorbar(data = gt[gt$measure == meas, ],
                    aes(x = lmbd, y = value, ymax = value + sd, ymin = value - sd),
                    color = "blue", width = 0.4) +
      geom_hline(yintercept = yintercept, color = clr) +
      ggtitle(toupper(meas)) +
      theme(plot.title = element_text(color = clr))
  }

  p1 <- effect_path(yhat_meas, y_meas, "nde", is.element("DE", BN))
  p2 <- effect_path(yhat_meas, y_meas, "nie", is.element("IE", BN))
  p3 <- effect_path(yhat_meas, y_meas, "expse_x1", is.element("SE", BN))
  p4 <- effect_path(yhat_meas, y_meas, "expse_x0", is.element("SE", BN))

  ret <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 4L)
}

