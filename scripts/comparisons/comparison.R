#!/usr/bin/env Rscript

#BSUB -W 24:00
#BSUB -n 32
#BSUB -R rusage[mem=1000]
#BSUB -o CFA_benchmark.out

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

exmp <- c("nomed", "med", "berkeley", "compas")
# Census -> need to handle mixed-variable W & Z

res <- list()
for (i in seq_along(exmp)) {
  res[[i]] <- method_cmp(example = exmp[i], nboot = 100, nsamp = 2000,
                         model = c("linear", "ranger"))
}

names(res) <- exmp
save(exmp, res, file = "CFA_benchmark.rda")

#'* visual analysis *
# load("CFA_benchmark.rda")
# ## comparisons
# vis_diff(res[["med"]])

## constraints
# check_constraints(exmp[["compas"]][["res"]])
