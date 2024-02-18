
#' @importFrom ggplot2 autoplot ggplot aes geom_density ggtitle geom_col
#' @importFrom ggplot2 scale_fill_discrete xlab scale_y_continuous
#' @importFrom ggplot2 geom_line geom_ribbon scale_color_discrete ylab xlab
#' @importFrom ggplot2 geom_errorbar geom_text theme_minimal position_fill theme
#' @importFrom ggplot2 element_rect scale_x_continuous geom_point theme_bw
#' @importFrom latex2exp TeX
#' @export
autoplot.fair_decision <- function(
    x, type = c("decision", "delta", "benefit_fairness"), n_bins = 10L,
    break_ties = TRUE, ...
) {

  type <- match.arg(type, c("decision", "delta", "benefit_fairness"))

  if (type == "decision") {

    ret <- autoplot(x$d_fcb, var_name = "d")
  } else if (type == "delta") {

    ret <- autoplot(x$delta_fcb, var_name = "\\Delta")
  } else if (type == "benefit_fairness") {

    res <- x$data[, c(x$X, x$D, "delta")]
    names(res) <- c("X", "D", "delta")
    if (break_ties) {

      min_diff <- min(diff(sort(res$delta))[diff(sort(res$delta)) > 0],
                      10^(-15), na.rm = TRUE)
      res$delta <- runif(seq_along(res$delta), max = min_diff / 10)
    }

    # group into bins
    res$bin <- .bincode(
      res$delta, quantile(res$delta, seq(0, 1, length.out = n_bins + 1L)),
      include.lowest = TRUE
    )

    pd_df <- aggregate(D ~ X + bin, data = res, mean, na.rm = TRUE)
    n_df <- aggregate(D ~ X + bin, data = res, function(x) length(x))
    names(pd_df)[3] <- "V1"
    names(n_df)[3] <- "V2"
    res <- merge(pd_df, n_df, by = c("X", "bin"))

    # plot
    ret <- ggplot(res, aes(x = bin, y = V1, color = factor(X))) +
      geom_line() + geom_point() + theme_bw() +
      geom_ribbon(
        aes(ymin = V1 - sqrt(1.96 * V1 * (1-V1) / V2),
            ymax = V1 + sqrt(1.96 * V1 * (1-V1) / V2),
            fill = factor(X)),
        linewidth = 0, alpha = 0.2
      ) +
      scale_x_continuous(breaks = 1:n_bins, labels = paste0("B", 1:n_bins)) +
      ylab("P(D = 1)") + xlab("Bin") +
      scale_color_discrete(name = x$X) +
      scale_fill_discrete(name = x$X) +
      ggtitle(paste("Benefit Fairness")) +
      theme(legend.position = "bottom", legend.box.background = element_rect())
  }

  ret
}

#' @export
predict.fair_decision <- function(x, newdata, budget = NULL, ...) {

  # predict delta on newdata
  delta <- predict_delta(x$xgb_mod, newdata, x$X, x$Z, x$W, x$D, x$delta_sign,
                         x$delta_transform)

  if (is.null(budget)) {

    decision <- NULL
  } else {

    thresh <- quantile(delta, prob = 1 - budget)
    decision <- as.integer((delta > 0) & (delta > thresh))
  }

  structure(
    list(
      delta = delta,
      decision = decision,
      test_data = newdata,
      X = x$X, Z = x$Z, W = x$W, Y = x$Y, D = x$D, x0 = x$x0, x1 = x$x1
    ), class = "fair_decision_test"
  )
}

#' @importFrom ggplot2 autoplot ggplot aes geom_density ggtitle geom_col
#' @importFrom ggplot2 scale_fill_discrete xlab scale_y_continuous
#' @importFrom ggplot2 geom_line geom_ribbon scale_color_discrete ylab xlab
#' @importFrom ggplot2 geom_errorbar geom_text theme_minimal position_fill theme
#' @importFrom ggplot2 element_rect scale_x_continuous geom_point theme_bw
#' @importFrom latex2exp TeX
#' @export
autoplot.fair_decision_test <- function(
    x, type = c("decision", "delta", "benefit_fairness"), n_bins = 10L,
    break_ties = TRUE, ...
) {

  type <- match.arg(type, c("decision", "delta", "benefit_fairness"))

  x$test_data[[x$D]] <- x$decision
  x$test_data[["delta"]] <- x$delta
  if (type == "decision") {

    test_d_fcb <- fairness_cookbook(
      x$test_data, X = x$X, Z = x$Z, W = x$W, Y = x$D, x0 = x$x0, x1 = x$x1,
      ...
    )
    ret <- autoplot(test_d_fcb, var_name = "d")
  } else if (type == "delta") {

    test_delta_fcb <- fairness_cookbook(
      x$test_data, X = x$X, Z = x$Z, W = x$W, Y = "delta", x0 = x$x0, x1 = x$x1,
      ...
    )
    ret <- autoplot(test_delta_fcb, var_name = "\\Delta")
  } else if (type == "benefit_fairness") {

    res <- x$test_data[, c(x$X, x$D, "delta")]
    names(res) <- c("X", "D", "delta")
    if (break_ties) {

      min_diff <- min(diff(sort(res$delta))[diff(sort(res$delta)) > 0],
                      10^(-15), na.rm = TRUE)
      res$delta <- runif(seq_along(res$delta), max = min_diff / 10)
    }

    # group into bins
    res$bin <- .bincode(
      res$delta, quantile(res$delta, seq(0, 1, length.out = n_bins + 1L)),
      include.lowest = TRUE
    )

    pd_df <- aggregate(D ~ X + bin, data = res, mean, na.rm = TRUE)
    n_df <- aggregate(D ~ X + bin, data = res, function(x) length(x))
    names(pd_df)[3] <- "V1"
    names(n_df)[3] <- "V2"
    res <- merge(pd_df, n_df, by = c("X", "bin"))

    # plot
    ret <- ggplot(res, aes(x = bin, y = V1, color = factor(X))) +
      geom_line() + geom_point() + theme_bw() +
      geom_ribbon(
        aes(ymin = V1 - sqrt(1.96 * V1 * (1-V1) / V2),
            ymax = V1 + sqrt(1.96 * V1 * (1-V1) / V2),
            fill = factor(X)),
        linewidth = 0, alpha = 0.2
      ) +
      scale_x_continuous(breaks = 1:n_bins, labels = paste0("B", 1:n_bins)) +
      ylab("P(D = 1)") + xlab("Bin") +
      scale_color_discrete(name = x$X) +
      scale_fill_discrete(name = x$X) +
      ggtitle(paste("Benefit Fairness")) +
      theme(legend.position = "bottom", legend.box.background = element_rect())
  }

  ret
}

# helpers
predict_delta <- function(xgb_mod, data, X, Z, W, D, delta_sign,
                          delta_transform) {

  data0 <- data1 <- data
  data0[[D]] <- 0
  data1[[D]] <- 1

  yd0 <- predict(xgb_mod, as.matrix(data0[, c(X, Z, W, D)]))
  yd1 <- predict(xgb_mod, as.matrix(data1[, c(X, Z, W, D)]))

  if (delta_sign == 1) {

    yd1[yd1 < yd0] <- yd0[yd1 < yd0]
  } else if (delta_sign == -1) {

    yd1[yd1 > yd0] <- yd0[yd1 > yd0]
  }

  delta_transform(yd1) - delta_transform(yd0)
}
