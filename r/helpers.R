
RejectOption <- function(prob, cl) {
  
  rate_cl <- tapply(prob, cl, function(x) mean(x > 0.5))
  fav_cl <- names(rate_cl)[which.max(rate_cl)]
  
  theta <- 0.5 + seq(0, 0.5, 0.005)
  tv <- vapply(
    theta,
    function(th) {
      mean(prob[cl == fav_cl] > th) - mean(prob[cl != fav_cl] > 1 - th) 
    }, numeric(1L)
  )
  
  theta_min <- theta[which.min(abs(tv))]
  
  prob[cl == fav_cl] <- prob[cl == fav_cl] > theta_min
  prob[cl != fav_cl] <- prob[cl != fav_cl] > 1 - theta_min
  
  prob
  
}
