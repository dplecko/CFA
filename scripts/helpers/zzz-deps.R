
pkgs <- c("causalweight", "readr", "ranger", "ggplot2",
          "latex2exp", "stringr", "grf", "assertthat", "data.table")

if (!all(vapply(pkgs, requireNamespace, logical(1L)))) {
  stop("Packages {pkgs} are required in order to proceed.")
  if (!interactive()) q("no", status = 1, runLast = FALSE)
}

library(causalweight)
library(readr)
library(ranger)
library(ggplot2)
library(latex2exp)
library(stringr)
library(grf)
library(assertthat)
library(data.table)
