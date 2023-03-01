
test_that("synthetic examples work", {
  
  res <- list(list(), list(), list())
  for (i in c(1, 2, 3)) {
    
    dat_fun <- ifelse(i == 1, ex_nomed, ifelse(i == 2, ex_med, ex_med_spur))
    nsamp <- 1000
    data <- with_seed(
      203, dat_fun(nsamp)
    )
    
    gtruth <- with_seed(203, dat_fun(nsamp, type = "gtruth"))
    
    res[[i]][["gt"]] <- gtruth[sort(names(gtruth))]
    
    Z <- grep("Z", names(data), value = TRUE)
    W <- grep("W", names(data), value = TRUE)
    
    for (method in c("medDML", "causal_forest")) { #
      
      mod <- with_seed(
        203,
        fairness_cookbook(data, X = "X", Z = Z, Y = "Y", W = W,
                          x0 = 0, x1 = 1, method = method)
      )
      
      res[[i]][[method]] <- summary(mod)$measures$value
      names(res[[i]][[method]]) <- summary(mod)$measures$measure
    }
  }
  
  barplot_slice <- function(res, i) {
    
    eff <- do.call(rbind, lapply(res[[i]], function(x) sapply(x, `[[`, 1L)))
    
    barplot(eff,
            main = "Synthetic Data Effect Estimates",
            xlab = "Effect",
            col = seq_len(nrow(eff)), beside = TRUE
    )
    
    legend("topleft",
           rownames(eff),
           fill = seq_len(ncol(eff))
    )
  }
  
  announce_snapshot_file(name = "synth1.png")
  announce_snapshot_file(name = "synth2.png")
  announce_snapshot_file(name = "synth3.png")
  
  expect_snapshot_plot("synth1", barplot_slice(res, 1L))
  expect_snapshot_plot("synth2", barplot_slice(res, 2L))
  expect_snapshot_plot("synth3", barplot_slice(res, 3L))
})



