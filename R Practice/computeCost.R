computeCost <- function(X, y, theta){
  m = length(y)
  return ((1/(2*m)) * sum((X %*% theta - y) ^ 2))
}