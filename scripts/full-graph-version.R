
library(drf)
library(ggplot2)

n <- 5 * 10^3
set.seed(2022)

scm <- list(
  X = list(
    pa = c(),
    fun = function(eps, uw) as.integer(uw < 0.5),
    eps = rnorm(n), uw = runif(n),
    value = NULL
  ),
  W1 = list(
    pa = c("X"),
    fun = function(X, eps, uw)
      (uw < 0.2 + X * 0.6) + eps,
    eps = rnorm(n), uw = runif(n), value = NULL
  ),
  W2 = list(
    pa = c("X", "W1"),
    fun = function(X, W1, eps, uw) W1^2 / 2 - 1 +
      (uw < 0.3 + X * 0.6) * (W1 / 2) + eps,
    eps = rnorm(n), uw = runif(n), value = NULL
  ),
  Y = list(
    pa = c("X", "W1", "W2"),
    fun = function(X, W1, W2, eps, uw) W1 * W2 / 6 +
      (uw < 0.2 + X * 0.6) * (W2 / 4 - 2) + eps,
    eps = rnorm(n), uw = runif(n), value = NULL
  )
)

l2_gen <- function(scm, x = replicate(length(scm), NULL)) {

  for (i in seq_along(scm)) {

    #' * get parents *
    pa <- scm[[i]]$pa
    args <- c(lapply(scm, `[[`, "value")[pa],
              list(eps = scm[[i]]$eps, uw = scm[[i]]$uw))

    #' * get f_i() *
    fv <- scm[[i]]$fun

    #' * check if intervention *
    if (is.element("X", pa) & !is.null(x[[i]])) args[["X"]] <- x[[i]]
    scm[[i]]$value <- do.call(fv, args)
  }

  as.data.frame(do.call(cbind, lapply(scm, `[[`, "value")))
}

drf_mc <- function(data, scm, x) {

  int.data <- data
  for (i in seq_along(scm)) {

    pa <- scm[[i]]$pa
    if (length(pa) == 0) next

    #' * fit on obs data *
    drf.fit <- drf(X = data[, pa, drop = FALSE],
                   Y = data[, names(scm)[i], drop = FALSE])


    if (is.element("X", pa) & !is.null(x[[i]])) int.data$X <- x[[i]]
    wgh <- get_sample_weights(drf.fit, newdata = int.data)
    idx <- vapply(seq_len(nrow(data)), function(k) sample.int(ncol(wgh), 1, prob = wgh[k, ]),
                  integer(1L))

    int.data[[names(scm)[i]]] <- data[[names(scm)[i]]][idx]
  }

  int.data
}

data <- l2_gen(scm)
x_vec <- list(1, 1, 0, 1)

y_mc <- drf_mc(data, scm, x_vec)$Y

y_tr <- l2_gen(scm, x_vec)$Y

df <- data.frame(y_mc, y_tr)

ggplot(df, aes(x = y_mc, y = y_tr)) +
  geom_point() + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")
