
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

dat <- get_data("compas")

library(faircause)

fcc <- fairness_cookbook(dat, X = "race", Z = c("sex", "age"), 
                  W = c("juv_fel_count", "juv_misd_count", "juv_other_count",
                        "priors_count", "c_charge_degree"),
                  Y = "two_year_recid", x0 = "White", x1 = "Non-White",
                  model = "linear")

summary(fcc)
autoplot(fcc, decompose = "xspec", dataset = "COMPAS")
ggsave(file.path(root, "misc", "poster", "compas.png"),
       width = 8, height = 8 * 2/3)
