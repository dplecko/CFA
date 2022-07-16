
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "R")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

datasets <- "census" # c("compas", "census")

for(dataset in datasets) {

  # Step 1 - load the data
  dat <- get_data(dataset)

  # Step 2 - load the metadata
  mdat <- get_metadata(dataset)

  # Step 3 - decompose Total Variation
  tvd <- fairness_cookbook(dat, X = mdat[["X"]], W = mdat[["W"]],
                           Z = mdat[["Z"]], Y = mdat[["Y"]], x0 = mdat[["x0"]],
                           x1 = mdat[["x1"]])

  # Step 4 - visualize the results
  autoplot(tvd, decompose = "xspec", dataset = dataset)

  # Step 5 - save the plot
  ggsave(file.path(root, "paper", paste0(dataset, ".png")), height = 10,
         width = 10, bg = "white")

}
