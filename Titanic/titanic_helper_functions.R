sigmoid <- function(z) {
  return (1 / (1 + exp(1) ** (-z)))
}


# Maps two features to a set of new features depending on degree.
mapFeature <- function(featureA, featureB, degree) {
  X_out <- matrix(c(1), nrow=length(featureA), ncol=28)
  current <- 2
  
  for (i in 1:degree) {
    for (j in 0:i) {
      X_out[, current] <- (featureA ^ (i - j)) * (featureB ^ j)
      current <- current + 1
    }
  }
  
  return (X_out)
}


costFunc <- function(theta, X, y) {
  m <- length(y)
  J <- (1/m) * (t(-y) %*% log(sigmoid(X %*% theta)) - t(1 - y) %*% log(1 - sigmoid(X %*% theta)))
  return (J)
}


gradFunc <- function(theta, X, y) {
  m <- length(y)
  grad <- (1/m) * t(X) %*% (sigmoid (X %*% theta) - y)
  return (grad)
}


costFuncReg <- function(theta, X, y, lambda) {
  m <- length(y)
  J <- (1/m) * (t(-y) %*% log(sigmoid(X %*% theta)) - t(1 - y) %*% log(1 - sigmoid(X %*% theta)))
  J <- J + (lambda / (2*m)) * (t(tail(theta, 2)) %*% tail(theta, 2))
  return (J)
}


gradFuncReg <- function(theta, X, y, lambda) {
  m <- length(y)
  n <- length(X[1,])
  grad <- (1/m) * t(X) %*% (sigmoid (X %*% theta) - y)
  grad <- c(head(grad,1), tail(grad, n-1) + (lambda/m)*tail(theta,n-1))
  return (grad)
}


predict <- function(X, theta) {
  return (sigmoid(X %*% theta) >= 0.5)
}



