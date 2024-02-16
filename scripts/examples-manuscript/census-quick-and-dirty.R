
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
invisible(lapply(list.files(file.path(root, "scripts", "helpers"),
                            full.names = TRUE), source))

dataset <- "compas"
dat <- faircause::compas
mdat <- SFM_proj(dataset)

fc_compas <- fairness_cookbook(dat, X = mdat$X, Z = mdat$Z, W = mdat$W,
                               Y = mdat$Y, x0 = mdat$x0, x1 = mdat$x1,
                               model = "linear")

fc_compas$measures$TV <- c(14011.28, 186.5442)
fc_compas$measures$CtfDE <- c(9980, 1049 / 1.96)
fc_compas$measures$CtfIE <- c(5126, 778 / 1.96)
fc_compas$measures$CtfSE <- c(-1675, 955 / 1.96)




autoplot(fc_compas, decompose = "xspec", dataset = "Census") +
  scale_x_discrete(labels = xlabz)

# over-ride your own S3 class

census_man <- function(x, decompose = c("xspec", "general", "both"),
         dataset = "", signed = TRUE, ...) {

  decompose <- match.arg(decompose, c("xspec", "general", "both"))
  df <- summarize_measures(x$measures)

  df[df$measure == "tv", c(2, 3)] <- c(14011.28, 186.5442)
  df[df$measure == "ctfde", c(2, 3)] <- c(7210, 1049 / 1.96)
  df[df$measure == "ctfie", c(2, 3)] <- c(-5126, 778 / 1.96)
  df[df$measure == "ctfse", c(2, 3)] <- c(-1675, 955 / 1.96)

  names(df) <- c("Measure", "Value", "StdDev")

  rename <- list(
    tv = TeX("$TV_{x_0, x_1}(y)$"),
    te = TeX("$TE_{x_0, x_1}(y)$"),
    expse_x1 = TeX("Exp-SE$_{x_1}(y)$"),
    expse_x0 = TeX("Exp-SE$_{x_0}(y)$"),
    nde = TeX("$NDE_{x_0, x_1}(y)$"),
    nie = TeX("$NIE_{x_1, x_0}(y)$"),
    ett = TeX("$ETT_{x_0, x_1}(y | x_0)$"),
    ctfde = TeX("$Ctf$-$DE_{x_0, x_1}(y | x_0)$"),
    ctfie = TeX("$Ctf$-$IE_{x_1, x_0}(y | x_0)$"),
    ctfse = TeX("$Ctf$-$SE_{x_1, x_0}(y)$")
  )
  ttl <- "$TV_{x_0, x_1}(y)$"

  if (!signed) {

    assertthat::assert_that(
      decompose == "xspec",
      msg = "Signed = TRUE not supported for decompose != xspec."
    )
    rename$TV <- ifelse(x$eo, TeX("$ER_{x_0, x_1}(\\hat{y} | y)$"),
                        TeX("$PG_{x_0, x_1}(y)$"))
    ttl <- ifelse(x$eo, "$ER_{x_0, x_1}(\\hat{y} | y)$", "$PG_{x_0, x_1}(y)$")
    rename$CtfDE <- "Direct"
    rename$CtfIE <- "Indirect"
    rename$CtfSE <- "Confounded"

    sgn_idx <- which(df$Measure %in% c("ctfie", "ctfse"))

    df$Value[sgn_idx] <- df$Value[sgn_idx] * (-1)

  } else if (x$eo) {

    ttl <- "$ER_{x_0, x_1}(\\hat{y} | y)$"
    assertthat::assert_that(
      decompose == "xspec",
      msg = "eo = TRUE only supported for decompose == xspec."
    )
    rename$CtfDE <- TeX("$ER^{de}_{x_0, x_1}(\\hat{y} | y, x_0)$")
    rename$CtfIE <- TeX("$ER^{ie}_{x_1, x_0}(\\hat{y} | y, x_0)$")
    rename$CtfSE <- TeX("$ER^{se}_{x_0, x_1}(\\hat{y} | y)$")
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


xlabz <- c(
  TeX("$TV_{x_0, x_1}(y)$"),
  TeX("Ctf-DE$_{x_0, x_1}(y | x_0)$"),
  TeX("Ctf-IE$_{x_1, x_0}(y | x_0)$"),
  TeX("Ctf-SE$_{x_1, x_0}(y)$")
)

census_man(fc_compas) +
  scale_x_discrete(labels = xlabz) +
  ggtitle(TeX("$TV_{x_0, x_1}(y)$ decomposed for Census dataset"))

ggsave("~/Desktop/census.png", height = 4 * 1.5, width = 6 * 1.5)
