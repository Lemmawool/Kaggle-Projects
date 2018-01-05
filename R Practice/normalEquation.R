normalEquation <- function(X, y){
  tX <- t(X)
  return (solve(t(X) %*% X) %*% t(X) %*% y)
}


