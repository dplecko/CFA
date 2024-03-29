#' @export
print.faircause <- function(x, ...) {
  cat("faircause object:\n\n")
  cat("Attribute:       ", x$X, "\n")
  cat("Outcome:         ", x$Y, "\n")
  cat("Confounders:     ", paste(x$Z, collapse = ","), "\n")
  cat("Mediators:       ", paste(x$W, collapse = ","), "\n")
}

#' @export
summary.faircause <- function(object, decompose = "xspec", ...) {

  structure(
    list(
      cl = object$cl,
      X = object$X,
      Z = object$Z,
      W = object$W,
      Y = object$Y,
      x0 = object$x0,
      x1 = object$x1,
      measures = summarize_measures(object$measures),
      decompose = decompose
    ),
    class = "summary.faircause"
  )
}

#' @export
print.summary.faircause <- function(x,
                                    digits = max(3L, getOption("digits") - 3L),
                                    ...) {

  cat("faircause object summary: \n")
  cat("\nCall:\n", paste(deparse(x$cl), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  cat("Protected attribute:                 ", x$X, "\n", sep = "")
  cat("Protected attribute levels:          ",
       paste(c(x$x0, x$x1), collapse = ", "), "\n", sep = "")

  meas_list <- lapply(x$measures$value, function(a) a)
  names(meas_list) <- x$measures$measure

  cat("Total Variation (TV):", meas_list$tv[1], "\n")

  cat("TV decomposition(s) :\n\n")
  if (x$decompose %in% c("general", "both")) {
    cat("TV_x0x1(y) (", meas_list$tv[1], ") = NDE_x0x1(y) (",
        meas_list$nde[1], ") - NIE_x1x0(y) (", meas_list$nie[1],
        ") + ExpSE_x0(y) (", meas_list$expse_x0[1], ") - ExpSE_x1(y) (",
        meas_list$expse_x1[1], ")\n", sep = "")
  }
  if (x$decompose %in% c("xspec", "both")) {
    cat("TV_x0x1(y) (", meas_list$tv[1], ") = CtfDE_x0x1(y | x0) (",
        meas_list$ctfde[1], ") - CtfIE_x1x0(y | x0) (", meas_list$ctfie[1],
        ") - CtfSE_x1x0(y | x0) (", meas_list$ctfse[1], ")\n", sep = "")
  }

  invisible(x)
}

#' @importFrom ggplot2 autoplot ggplot aes geom_density ggtitle geom_col
#' @importFrom ggplot2 scale_fill_discrete xlab scale_y_continuous
#' @importFrom ggplot2 geom_errorbar geom_text theme_minimal position_fill theme
#' @importFrom ggplot2 element_text scale_x_discrete
#' @importFrom latex2exp TeX
#' @export
autoplot.faircause <- function(object, decompose = c("xspec", "general", "both"),
                               dataset = "", signed = TRUE, var_name = "y", ...) {

  decompose <- match.arg(decompose, c("xspec", "general", "both"))
  df <- summarize_measures(object$measures)
  names(df) <- c("Measure", "Value", "StdDev")

  rename <- list(
    tv = TeX(sprintf("$TV_{x_0, x_1}(%s)$", var_name)),
    te = TeX(sprintf("$TE_{x_0, x_1}(%s)$", var_name)),
    expse_x1 = TeX(sprintf("$Exp$-$SE_{x_1}(%s)$", var_name)),
    expse_x0 = TeX(sprintf("$Exp$-$SE_{x_0}(%s)$", var_name)),
    nde = TeX(sprintf("$NDE_{x_0, x_1}(%s)$", var_name)),
    nie = TeX(sprintf("$NIE_{x_1, x_0}(%s)$", var_name)),
    ett = TeX(sprintf("$ETT_{x_0, x_1}(%s | x_0)$", var_name)),
    ctfde = TeX(sprintf("$Ctf$-$DE_{x_0, x_1}(%s | x_0)$", var_name)),
    ctfie = TeX(sprintf("$Ctf$-$IE_{x_1, x_0}(%s | x_0)$", var_name)),
    ctfse = TeX(sprintf("$Ctf$-$SE_{x_1, x_0}(%s)$", var_name))
  )
  ttl <- sprintf("$TV_{x_0, x_1}(%s)$", var_name)

  if (!signed) {

    assertthat::assert_that(
      decompose == "xspec",
      msg = "Signed = TRUE not supported for decompose != xspec."
    )
    rename$TV <- TeX("$PG_{x_0, x_1}(y)$")
    ttl <- "$PG_{x_0, x_1}(y)$"
    rename$CtfDE <- "Direct"
    rename$CtfIE <- "Indirect"
    rename$CtfSE <- "Confounded"

    sgn_idx <- which(df$Measure %in% c("ctfie", "ctfse"))

    df$Value[sgn_idx] <- df$Value[sgn_idx] * (-1)

  }

  inc_meas <- switch(
    decompose,
    xspec = c("tv", "ctfde", "ctfie", "ctfse"),
    general = c("tv", "nde", "nie", "expse_x0", "expse_x1"),
    both = names(rename)
  )

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

# helpers
summarize_measures <- function(meas) {

  data.frame(
    measure = sort(unique(meas$measure)),
    value = tapply(meas$value, meas$measure, mean),
    sd = tapply(meas$value, meas$measure, sd)
  )
}
