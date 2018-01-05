featureNormalize <- function(X) {
  X_columns = length(X[1,])
  mu = rep(0, X_columns)
  X_norm = X
  sigma = mu
  
  for (i in 1:X_columns){
    mu[i] = mean(X[,i])
    sigma[i] = sd(X[,i])
    X_norm[,i] = (X[,i] - mu[i]) / sigma[i]
  }
  
  return (X_norm)
}