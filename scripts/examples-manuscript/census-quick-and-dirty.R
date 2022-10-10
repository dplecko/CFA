
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
invisible(lapply(list.files(file.path(root, "scripts", "helpers"),
                            full.names = TRUE), source))

dataset <- "compas"
dat <- faircause::compas
mdat <- get_metadata(dataset)

fc_compas <- fairness_cookbook(dat, X = mdat$X, Z = mdat$Z, W = mdat$W,
                               Y = mdat$Y, x0 = mdat$x0, x1 = mdat$x1,
                               model = "linear")

fc_compas$measures$TV <- c(14011.28, 186.5442)
fc_compas$measures$CtfDE <- c(9980, 1049 / 1.96)
fc_compas$measures$CtfIE <- c(5126, 778/ 1.96)
fc_compas$measures$CtfSE <- c(-1675, 955/ 1.96)


xlabz <- c(
  TeX("$TV_{x_0, x_1}(y)$"),
  TeX("$x$-$DE_{x}(y | x_0)$"),
  TeX("$x$-$IE_{x}(y | x_0)$"),
  TeX("$x$-$SE_{x_1, x_0}(y)$")
)

autoplot(fc_compas, decompose = "xspec", dataset = "Census") +
  scale_x_discrete(labels = xlabz)
