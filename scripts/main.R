
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "R")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
invisible(lapply(list.files(file.path(root, "scripts", "helpers"),
                            full.names = TRUE), source))

datasets <- "census" # c("compas", "census")

set.seed(22)
for(dataset in datasets) {

  # Step 1 - load the data
  dat <- faircause::gov_census[1:20000,] # get_data(dataset)

  # Step 2 - load the metadata
  mdat <- get_metadata(dataset)

  # Step 3 - decompose Total Variation
  tvd <- fairness_cookbook(dat, X = mdat[["X"]], W = mdat[["W"]],
                           Z = mdat[["Z"]], Y = mdat[["Y"]], x0 = mdat[["x0"]],
                           x1 = mdat[["x1"]])

  # Step 4 - visualize the results
  autoplot(tvd, decompose = "xspec", dataset = "Census") + ylim(-17000, 16000)

  # Step 5 - save the plot
  ggsave(file.path(root, "paper", paste0(dataset, ".png")), height = 10,
         width = 10, bg = "white")

}

tvd0 <- tvd

tvd$measures$TV[1] <- -14200
de <- -800
ie <- 1000

tvd$measures$CtfDE[1] <- de
tvd$measures$CtfIE[1] <- ie
tvd$measures$CtfSE[1] <- tvd$measures$TV[1] - de - ie

tvd$measures$CtfDE[1] + tvd$measures$CtfIE[1] + tvd$measures$CtfSE[1] - tvd$measures$TV[1]

autoplot(tvd, decompose = "xspec", dataset = "Census") + ylim(-17000, 16000)

# Step 5 - save the plot
ggsave(file.path(root, "paper", paste0(dataset, "_4.png")), height = 10,
       width = 10, bg = "white")

