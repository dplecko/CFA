
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

dataset <- "compas"

dat <- get_data(dataset)
mdat <- get_metadata(dataset)

fc_compas <- fairness_cookbook(dat, X = mdat$X, Z = mdat$Z, W = mdat$W, 
                               Y = mdat$Y, x0 = mdat$x0, x1 = mdat$x1,
                               model = "linear")

cat("Ctf-IE = ", spec_dec(100 * fc_compas$measures$CtfIE[1], 1), "\\%",
    " \\pm ", spec_dec(100 * fc_compas$measures$CtfIE[2], 1), "\\%",  sep = "")

cat("Ctf-SE = ", spec_dec(100 * fc_compas$measures$CtfSE[1], 1), "\\%",
    " \\pm ", spec_dec(100 * fc_compas$measures$CtfSE[2], 1), "\\%",  sep = "")
