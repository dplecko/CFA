
library(quarto)

# render Index
quarto_render("quarto-webpage/index.qmd")

# Other Tasks
quarto_render("quarto-webpage/pages/python-use.qmd")
quarto_render("quarto-webpage/pages/ci-tests.Rmd")

# Task 1
quarto_render("quarto-webpage/pages/t1-census.Rmd")
quarto_render("quarto-webpage/pages/t1-admissions-over-time.Rmd")
quarto_render("quarto-webpage/pages/t1-compas-y-yhat.Rmd")
quarto_render("quarto-webpage/pages/t1-compas-beyond-sfm.Rmd")
quarto_render("quarto-webpage/pages/t1-icu-mortality.Rmd")

# Task 2
quarto_render("quarto-webpage/pages/t2-compas-fpt-inthe-wild.Rmd")
quarto_render("quarto-webpage/pages/t2-compas-neural-inproc.Rmd")

# Task 3
quarto_render("quarto-webpage/pages/t3-surgeries.qmd")
quarto_render("quarto-webpage/pages/t3-mimic-respirators.Rmd")
