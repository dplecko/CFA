#!/usr/bin/env Rscript

#BSUB -W 24:00
#BSUB -n 32
#BSUB -R rusage[mem=1000]
#BSUB -o CFA_benchmark.out

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

#'* synthetic data experiments *

## nomed
res_nomed <- method_cmp(example = "nomed",
                        nboot = 2, nsamp = 200, 
                        model = "ranger") 

## med
res_med <- method_cmp(example = "med",
                      nboot = 5, nsamp = 500, 
                      model = "ranger")

#'* real data experiments *

# COMPAS comparison
res_compas <- method_cmp(example = "compas",
                         nboot = 2, nsamp = 500, 
                         model = "ranger")

save(res_med, res_nomed, res_compas, file = "CFA_benchmark.rda")
# Berkeley comparison -> need to handle Z empty

# Census -> need to handle mixed-variable W & Z

#'* visual analysis *

# ## comparisons
# vis_diff(res_nomed)
# vis_diff(res_med)
# vis_diff(res_compas)
# 
# ## constraints
# check_constraints(res_nomed)
# check_constraints(res_med)
# check_constraints(res_compas)
