gradientDescent <- function(X, y, theta, alpha, iterations) {
  m <- length(y)
  temp <- X[1,]
  
  for (i in 1:iterations) {
    
    for (j in 1:length(X[1,])){
      temp[j] <- theta[j] - alpha * (1/m) * sum(t((X %*% theta - y)) %*% X[,j])
    }
    theta <- as.matrix(temp)
  }
  
  return (theta)
}