
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

dataset <- "census"

dat <- faircause::gov_census
mdat <- get_metadata(dataset)

x0_ind <- dat[, mdat[["X"]]] == mdat[["x0"]]

obs_par <- switch(
  2L - is.na(mdat[["ylvl"]]),
  mean(dat[!x0_ind, mdat[["Y"]]]) - mean(dat[x0_ind, mdat[["Y"]]]),
  mean(dat[!x0_ind, mdat[["Y"]]] == mdat[["ylvl"]]) -
    mean(dat[x0_ind, mdat[["Y"]]] == mdat[["ylvl"]])
)

switch(
  2L - is.na(mdat[["ylvl"]]),
  cat("\\ex[Y \\mid x_1] - \\ex[Y \\mid x_0] =", round(obs_par)),
  cat("P(Y \\mid x_1) - P(Y \\mid x_0) =", round(obs_par, 3))
)
