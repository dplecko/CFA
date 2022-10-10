#' @export
print.faircause <- function(x, ...) {
  cat("faircause object:\n\n")
  cat("Attribute:       ", x$X, "\n")
  cat("Outcome:         ", x$Y, "\n")
  cat("Confounders:     ", paste(x$Z, collapse = ","), "\n")
  cat("Mediators:       ", paste(x$W, collapse = ","), "\n")
}

#' @export
summary.faircause <- function(object, decompose = "general", ...) {
  cat("faircause object summary: \n")
  cat("\nCall:\n", paste(deparse(object$cl), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  cat("Total Variation (TV):", object$measures$TV[1], "\n")

  cat("TV decomposition(s) :\n\n")
  if (decompose %in% c("general", "both")) {
    cat("TV_x0x1(y) (", object$measures$TV[1], ") = NDE_x0x1(y) (",
        object$measures$NDE[1], ") - NIE_x1x0(y) (", object$measures$NIE[1],
        ") + ExpSE_x0(y) (", object$measures$ExpSE_x0[1], ") - ExpSE_x1(y) (",
        object$measures$ExpSE_x1[1], ")\n", sep = "")
  }
  if (decompose %in% c("xspec", "both")) {
    cat("TV_x0x1(y) (", object$measures$TV[1], ") = CtfDE_x0x1(y) (",
        object$measures$DE[1], ") - CtfIE_x1x0(y) (", object$measures$IE[1],
        ") - CtfSE_x1x0(y) (", object$measures$SE[1], ")\n", sep = "")
  }
}

#' @importFrom ggplot2 autoplot ggplot aes geom_density ggtitle
#' @importFrom ggplot2 scale_fill_discrete xlab scale_y_continuous
#' @importFrom ggplot2 geom_bar geom_text theme_minimal position_fill theme
#' @importFrom latex2exp TeX
#' @export
autoplot.faircause <- function(x, decompose = "general", dataset = "",
                               signed = TRUE, eo = FALSE, ...) {
  df <- data.frame(names(x$measures), Reduce(rbind, x$measures))
  names(df) <- c("Measure", "Value", "StdDev")
  inc_meas <- c("TV")
  rename <- list(
    TV = TeX("$TV_{x_0, x_1}(y)$"),
    TE = TeX("$TE_{x_0, x_1}(y)$"),
    ExpSE_x1 = TeX("$Exp$-SE_{x_1}(y)$"),
    ExpSE_x0 = TeX("$Exp$-SE_{x_0}(y)$"),
    NDE = TeX("$NDE_{x_0, x_1}(y)$"),
    NIE = TeX("$NIE_{x_1, x_0}(y)$"),
    ETT = TeX("$ETT_{x_0, x_1}(y | x_0)$"),
    CtfDE = TeX("$Ctf$-$DE_{x_0, x_1}(y | x_0)$"),
    CtfIE = TeX("$Ctf$-$IE_{x_1, x_0}(y | x_0)$"),
    CtfSE = TeX("$Ctf$-$SE_{x_1, x_0}(y)$")
  )
  ttl <- "$TV_{x_0, x_1}(y)$"


  if (!signed) {

    assertthat::assert_that(
      decompose == "xspec",
      msg = "Signed = TRUE not supported for decompose != xspec."
    )
    rename$TV <- ifelse(eo, TeX("$ER_{x_0, x_1}(\\hat{y} | y_0)$"),
                        TeX("$PG_{x_0, x_1}(y)$"))
    ttl <- ifelse(eo, "$ER_{x_0, x_1}(\\hat{y} | y_0)$", "$PG_{x_0, x_1}(y)$")
    rename$CtfDE <- "Direct"
    rename$CtfIE <- "Indirect"
    rename$CtfSE <- "Confounded"

    sgn_idx <- which(df$Measure %in% c("CtfIE", "CtfSE"))

    df$Value[sgn_idx] <- df$Value[sgn_idx] * (-1)

  }

  if (decompose %in% c("general", "both")) {
    inc_meas <- c(inc_meas, "NDE", "NIE", "ExpSE_x0", "ExpSE_x1")
  } else if (decompose == "xspec") {
    inc_meas <- c(inc_meas, "CtfDE", "CtfIE", "CtfSE")
  } else inc_meas <- names(rename)

  df$Measure <- factor(df$Measure, levels = names(rename))
  df <- df[df$Measure %in% inc_meas, ]

  xlabz <- parse(text = unlist(rename[names(rename) %in% df$Measure]))

  ggplot(df, aes(x = Measure, y = Value, fill = Measure)) + geom_col() +
    theme_minimal() +
    geom_errorbar(
      aes(x = Measure, ymin = Value - 1.96*StdDev, ymax = Value + 1.96*StdDev),
      color = "black", width = 0.5
    ) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 16),
      axis.title.x = element_text(size = 18),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
      title = element_text(size = 20)
    ) + scale_x_discrete(labels = xlabz) +
    xlab("Causal Fairness Measure") +
    ggtitle(TeX(paste0(ttl, " decomposition ", dataset)))
}
