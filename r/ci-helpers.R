
msd_one <- function(x1, t1, meas, boots) {

  ms <- vapply(
    boots, function(ids) mean(x1[ids[[t1]]], na.rm = TRUE), numeric(1L)
  )

  data.frame(value = ms, boot = seq_along(boots), measure = meas) # c(mean(ms), sd(ms))
}

msd_two <- function(x1, t1, x2, t2, meas, boots) {

  ms <- vapply(
    boots,
    function(ids) {
      mean(x1[ids[[t1]]], na.rm = TRUE) +  mean(x2[ids[[t2]]], na.rm = TRUE)
    }, numeric(1L)
  )

  data.frame(value = ms, boot = seq_along(boots), measure = meas) # c(mean(ms), sd(ms))
}

msd_three <- function(x1, t1, x2, t2, x3, t3, meas, boots) {

  ms <- vapply(
    boots,
    function(ids) {
      mean(x1[ids[[t1]]], na.rm = TRUE) + mean(x2[ids[[t2]]], na.rm = TRUE) +
        mean(x3[ids[[t3]]], na.rm = TRUE)
    }, numeric(1L)
  )

  data.frame(value = ms, boot = seq_along(boots), measure = meas) # c(mean(ms), sd(ms))
}

inh_str <- function(x, meas, set0 = FALSE, setna = FALSE) {

  x$measure <- meas
  if (set0) x$value <- 0
  if (setna) x$value <- NA_real_

  x
}
