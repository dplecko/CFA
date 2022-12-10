
data_gen <- function(n, add_z = FALSE, seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  x <- rbinom(n, size = 1, prob = 0.5)

  coeff <- 1 / 4
  dev   <- 1

  w <-  -x * coeff + coeff / 2 + rnorm(n, sd = dev)
  y <- rbinom(n, size = 1, prob = expit(x))

  res <- data.frame(y = factor(y), x, w)

  if (add_z) {
    res <- cbind(res, z = rnorm(n))
  }

  res
}

tot_var <- function(x, what, var) {

  ind <- x[["base.ind"]][seq_len(nrow(x[[what]]))]
  dat <- x[[what]][[var]]

  if (is.factor(dat)) {
    dat <- as.numeric(as.character(dat))
  }

  signif(mean(dat[ind]) - mean(dat[!ind]))
}

sem <- function(f, a, e) {

  x <- vector("list", length(f))
  names(x) <- names(f)

  for (i in seq_along(f)) {
    x[[i]] <- f[[i]](a, x, e[, i])
  }

  as.data.frame(c(list(a = a), x))
}

save_png <- function(code, width = 400, height = 400) {

  path <- tempfile(fileext = ".png")

  png(path, width = width, height = height, type = "quartz",
      antialias = "none")

  on.exit(dev.off())

  code

  path
}

save_csv <- function(data) {

  path <- tempfile(fileext = ".csv")

  write.csv(data, path)

  path
}

expit <- function(x) exp(x)/(1+exp(x))

skip_on_r_version <- function(min_version = "3.6.0") {
  if (getRversion() < min_version) {
    skip(paste("R version <", min_version))
  }
}

compare_img_data <- function(old, new) {
  old <- magick::image_read(old)
  new <- magick::image_read(new)
  identical(magick::image_data(new), magick::image_data(old))
}

skip_if_not_on_arch <- function(arch) {

  curr_arch <- R.Version()$arch

  skip_if_not(
    identical(curr_arch, arch),
    message = paste("skipping on arch", curr_arch)
  )
}

expect_snapshot_plot <- function(name, code, mac_only = TRUE, arch = NULL) {

  if (mac_only) {
    skip_on_os(c("windows", "linux", "solaris"))
  }

  if (!is.null(arch)) {
    skip_if_not_on_arch(arch)
  }

  skip_if_not_installed("ggplot2", "3.0.0")

  path <- save_png(code)
  expect_snapshot_file(path, paste0(name, ".png"), compare = compare_img_data)
}

expect_snapshot_csv <- function(name, code, mac_only = TRUE, arch = NULL) {

  if (mac_only) {
    skip_on_os(c("windows", "linux", "solaris"))
  }

  if (!is.null(arch)) {
    skip_if_not_on_arch(arch)
  }

  skip_on_r_version("3.6.0")

  path <- save_csv(code)
  expect_snapshot_file(path, paste0(name, ".csv"))
}

expect_snapshot_json <- function(code, mac_only = TRUE, arch = NULL, ...) {

  if (mac_only) {
    skip_on_os(c("windows", "linux", "solaris"))
  }

  if (!is.null(arch)) {
    skip_if_not_on_arch(arch)
  }

  skip_on_r_version("3.6.0")

  expect_snapshot_value(code, style = "json2", ...)
}

expect_snapshot_code <- function(..., mac_only = TRUE, arch = NULL) {

  if (mac_only) {
    skip_on_os(c("windows", "linux", "solaris"))
  }

  if (!is.null(arch)) {
    skip_if_not_on_arch(arch)
  }

  expect_snapshot(...)
}

with_seed <- function(seed, code, rng_kind = "default",
                      rng_normal_kind = rng_kind, rng_sample_kind = rng_kind) {

  code <- substitute(code)

  old_seed <- get_seed()
  new_seed <- list(
    seed = seed,
    rng_kind = c(rng_kind, rng_normal_kind, rng_sample_kind)
  )

  if (is.null(old_seed)) {
    on.exit(rm_seed())
  } else {
    on.exit(set_seed(old_seed))
  }

  set_seed(new_seed)

  eval.parent(code)
}

# the following utilities originate from withr

has_seed <- function() {
  exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}

get_seed <- function() {

  if (!has_seed()) {
    return(NULL)
  }

  seed <- get(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)

  list(random_seed = seed, rng_kind = RNGkind())
}

set_seed <- function(seed) {

  if (getRversion() < "3.6") {
    seed$rng_kind <- seed$rng_kind[1L:2L]
  }
  if (is.null(seed$seed)) {
    do.call(RNGkind, args = as.list(seed$rng_kind))
    assign(".Random.seed", seed$random_seed, globalenv())
  } else {
    do.call(RNGkind, args = as.list(seed$rng_kind))
    set.seed(seed$seed)
  }
}

rm_seed <- function() {

  if (!has_seed()) {
    return(NULL)
  }

  set.seed(seed = NULL)

  rm(".Random.seed", envir = globalenv())
}
