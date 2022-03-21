#!/usr/bin/env Rscript

#BSUB -W 24:00
#BSUB -n 32
#BSUB -R rusage[mem=1000]
#BSUB -o CFA_benchmark.out

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

exmp <- list(
  nomed = list(
    example = "nomed", nboot = 100, nsamp = 2000, 
    model = c("linear", "ranger")
  ),
  med = list(
    example = "med", nboot = 100, nsamp = 2000, 
    model = c("linear", "ranger")
  ),
  compas = list(
    example = "compas", nboot = 100, nsamp = 2000,
    model = c("linear", "ranger")
  )
  # Berkeley comparison -> need to handle Z empty
  # Census -> need to handle mixed-variable W & Z
)

for (i in seq_along(exmp)) {
  exmp[[i]][["res"]] <- method_cmp(example = exmp[[i]][["example"]],
                                   nboot = exmp[[i]][["nboot"]], 
                                   nsamp = exmp[[i]][["nsamp"]], 
                                   model = exmp[[i]][["model"]])
}

save(exmp, file = "CFA_benchmark.rda")

#'* visual analysis *

# ## comparisons
# vis_diff(exmp[["compas"]][["res"]])

## constraints
# check_constraints(exmp[["compas"]][["res"]])

# method_cmp(example = "berkeley", nboot = 2, nsamp = 100, model = "ranger")
