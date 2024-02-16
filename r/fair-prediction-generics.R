
#' @importFrom ggplot2 autoplot ggplot aes geom_density ggtitle geom_col
#' @importFrom ggplot2 scale_fill_discrete xlab scale_y_continuous
#' @importFrom ggplot2 geom_errorbar geom_text theme_minimal position_fill theme
#' @importFrom ggplot2 element_text scale_x_discrete
#' @importFrom latex2exp TeX
#' @export
autoplot.fair_prediction <- function(x, type = c("causal", "accuracy"), ...) {
  
  type <- match.arg(type, c("causal", "accuracy"))
  
  eval_meas <- x$yhat_meas
  y_meas <- x$y_meas
  y_meas <- cbind(y_meas, lmbd = -0.5)
  
  if (type == "causal") {
    
    ret <- lambda_solution_path(eval_meas, y_meas, x$BN)
    
  } else if (type == "accuracy") {
    
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
  }
  
  ret
}


#' @export
predict.fair_prediction <- function(x, newdata, ...) {
  
  lmbd_seq <- x$lmbd_seq
  test_meas <- NULL
  preds <- replicate(length(lmbd_seq), NULL)
  names(preds) <- paste("Lambda =", lmbd_seq)
  y_meas <- x$y_meas
  y_meas <- cbind(y_meas, lmbd = -0.5)
  for (i in seq_along(lmbd_seq)) {
    
    lmbd <- lmbd_seq[i]
    
    # get the predictions on the
    newdata$preds <- preds[[i]] <- as.vector(
      reticulate::py$pred_nn_proba(x$neural_models[[i]],
                                   newdata[ , c(x$X, x$Z, x$W)])
    )
    test_fcb <- fairness_cookbook(newdata, X = x$X, Z = x$Z, W = x$W,
                                  Y = "preds", x0 = x$x0, x1 = x$x1,
                                  model = x$model, method = x$method,
                                  tune_params = x$tune_params,
                                  nboot1 = x$nboot1, nboot2 = x$nboot2, ...)
    
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
      BN = x$BN,
    ), class = "fair_prediction_test"
  )
}

#' @export
autoplot.fair_prediction_test <- function(x, ...) {
  
  lambda_solution_path(x$test_meas, x$y_meas, x$BN)
}

# helpers
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
      ggtitle(stringr::str_to_upper(meas)) +
      theme(plot.title = element_text(color = clr))
  }
  
  p1 <- effect_path(yhat_meas, y_meas, "nde", is.element("DE", BN))
  p2 <- effect_path(yhat_meas, y_meas, "nie", is.element("IE", BN))
  p3 <- effect_path(yhat_meas, y_meas, "expse_x1", is.element("SE", BN))
  p4 <- effect_path(yhat_meas, y_meas, "expse_x0", is.element("SE", BN))
  
  ret <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 4L)
}