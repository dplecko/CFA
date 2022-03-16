expit <- function(x) return(exp(x)/(1+exp(x)))

opportunity_gap <- function(Y.hat, labels, class){
  rate.class.0 <- sum(Y.hat == 1 & labels == 1 & class == 0) / sum(labels == 1 & class == 0)
  rate.class.1 <- sum(Y.hat == 1 & labels == 1 & class == 1) / sum(labels == 1 & class == 1)
  return(c(rate.class.0, rate.class.1))
}

direct_effect <- function(df, mu = )

generator <- function(n, mu = 0.55, c = 0.8, sigma.x = 2, sigma.n = 0.1){
  A <- rbinom(n, size = 1, prob = 0.5)
  X <- mu * (A == 0) + rnorm(n, sd = sigma.x)
  Y <- rbinom(n, size = 1, prob = expit(X + c*(A == 0) + rnorm(n, sd = sigma.n)))
  df <- data.frame(Y, A, X)
  return(df)
}

train.data <- generator(n = 100000)
#RF <- ranger::ranger(Y ~ ., data = train.data, classification = TRUE)
test.data <- generator(n = 100000)
class <- test.data$A

train.data$A <- test.data$A <- 0
Y.hat <- class::knn(train.data[, -1], test.data[,-1], train.data[, 1], k = 50)
1 - opportunity_gap(as.integer(Y.hat) - 1, test.data$Y, class)

# the direct effect (equals 0)
subset.0 <- class == 0 & test.data$Y == 1
subset.1 <- class == 1 & test.data$Y == 1

# measure the indirect effect
Y.hat <- class::knn(train = train.data[, -1], test = test.data[subset.0,-1], 
                     cl = train.data[, 1], k = 50, prob = TRUE)

S.hat <- attr(Y.hat, "prob")
S.hat[Y.hat == 0] <- 1 - S.hat[Y.hat == 0]

test.data.tilde <- test.data[subset.0, ]
test.data.tilde$X <- test.data.tilde$X - 0.55 
Y.hat.tilde <- class::knn(train = train.data[, -1], test = test.data.tilde[, -1], 
                          cl = train.data[, 1], k = 50, prob = TRUE)
S.hat.tilde <- attr(Y.hat.tilde, "prob")
S.hat.tilde[Y.hat.tilde == 0] <- 1 - S.hat.tilde[Y.hat.tilde == 0]
IDE <- sum(S.hat.tilde) / sum(subset.0) - sum(S.hat) / sum(subset.0)
IDE

# measure the spurious effect
Y.hat <- class::knn(train = train.data[, -1], test = test.data[subset.1,-1], 
  cl = train.data[, 1], k = 50, prob = TRUE)
S.hat <- attr(Y.hat, "prob")
S.hat[Y.hat == 0] <- 1 - S.hat[Y.hat == 0]

test.data.tilde <- test.data[subset.0, ]
test.data.tilde$X <- test.data.tilde$X - 0.55 
Y.hat.tilde <- class::knn(train = train.data[, -1], test = test.data.tilde[, -1], 
  cl = train.data[, 1], k = 50, prob = TRUE)
S.hat.tilde <- attr(Y.hat.tilde, "prob")
S.hat.tilde[Y.hat.tilde == 0] <- 1 - S.hat.tilde[Y.hat.tilde == 0]

SE <- sum(S.hat.tilde) / sum(subset.0) - sum(S.hat) / sum(subset.1)
SE


