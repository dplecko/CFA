
crf_wrap <- function(X, Y, W, mns, tune_params) {
  
  if (tune_params & is.null(mns)) {
    
    tune.parameters <- "min.node.size"
    mns <- 5L
  } else tune.parameters <- "none"
  
  if (is.null(mns)) mns <- 5L
  
  tryCatch(
    expr = {
      crf <- causal_forest(
        X = X, Y = Y, W = W, 
        min.node.size = mns, tune.parameters = tune.parameters
      )
    },
    error = function(e) {
      print(e)
      message(c('Caught an error in causal_forest, likely due to non-numeric',
                ' input. Switching to method = \"medDML\".'))
    },
    finally = NULL
  )
  
  if (exists("crf")) return(crf) else return(NULL)
}

ci_crf <- function(data, X, Z, W, Y, x0, x1, rep, nboot,
                   tune_params, params) {
  
  boot_samp <- if (rep > 1) sample.int(nrow(data), replace = TRUE) else
    seq_len(nrow(data))
  boot_data <- data[boot_samp, ]
  
  boots <- lapply(
    seq_len(nboot),
    function(i) {
      
      ind <- sample.int(nrow(boot_data), replace = TRUE)
      idx0 <- boot_data[[X]][ind] == x0
      list(all = ind, id0 = ind[idx0], id1 = ind[!idx0])
    }
  )

  y <- as.numeric(boot_data[[Y]]) - is.factor(boot_data[[Y]])
  tv <- msd_two(y, "id1", -y, "id0", "tv", boots)
  
  if (length(Z) == 0) {
    
    te <- inh_str(tv, "te")
    ett <- inh_str(tv, "ett")
    expse_x1 <- inh_str(tv, "expse_x1", set0 = TRUE)
    expse_x0 <- inh_str(tv, "expse_x0", set0 = TRUE)
    ctfse <- inh_str(tv, "ctfse", set0 = TRUE)
    crf_te <- list(predictions = rep(tv$value[1], nrow(data)))
  } else {
    
    crf_te <- crf_wrap(X = as.matrix(boot_data[, Z]), 
                       Y = as.vector(boot_data[[Y]]), 
                       W =  as.integer(boot_data[[X]]) - 1L, 
                       mns = params[["mns_te"]], tune_params)
    
    if (!is.null(crf_te)) {
      
      te <- msd_one(crf_te$predictions, "all", "te", boots)
      ett <- msd_one(crf_te$predictions, "id0", "ett", boots)
      ctfse <- msd_three(crf_te$predictions, "id0", -y, "id1", y, "id0", 
                         "ctfse", boots)
      expse_x0 <- inh_str(tv, "expse_x0", setna = TRUE)
      expse_x1 <- inh_str(tv, "expse_x1", setna = TRUE)
      params[["mns_te"]] <- crf_te$min.node.size
    } else return(NULL)
  }
  
  if (length(W) == 0) {
    
    nde <- inh_str(te, "nde")
    ctfde <- inh_str(ett, "ctfde")
    ctfie <- inh_str(ett, "ctfie", set0 = TRUE)
    nie <- inh_str(te, "nie", set0 = TRUE)
  } else {
    
    crf_med <- crf_wrap(X = as.matrix(boot_data[, c(Z, W)]), 
                        Y = as.vector(boot_data[[Y]]), 
                        W =  as.integer(data[[X]]) - 1L, 
                        mns = params[["mns_de"]], tune_params)
    
    if (!is.null(crf_med)) {
      
      nde <- msd_one(crf_med$predictions, "all", "nde", boots)
      ctfde <- msd_one(crf_med$predictions, "id0", "ctfde", boots)
      
      nie <- msd_two(crf_med$predictions, "all", -crf_te$predictions, "all",
                     "nie", boots)
      ctfie <- msd_two(crf_med$predictions, "id0", -crf_te$predictions, "id0",
                       "ctfie", boots)
      params[["mns_de"]] <- crf_med$min.node.size
    } else return(NULL)
  }
  
  res <- do.call(
    rbind,
    list(tv, te, expse_x1, expse_x0, ett, ctfse, nde, nie, ctfde, ctfie)
  )
  res <- data.frame(res, rep = rep)
  attr(res, "params") <- params
  res
}
