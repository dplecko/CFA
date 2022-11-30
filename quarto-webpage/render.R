
setwd("~/quarto-dummy")
library(quarto)
quarto_render("shai-challenge.Rmd")
quarto_render("index.qmd")

quarto_render("pages/ci-tests.Rmd")

quarto_render("pages/compas-t1.Rmd")

quarto_render("pages/compas-t2.Rmd")
