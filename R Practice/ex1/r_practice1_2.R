# The following code uses gradient decent with feature normalization and
# the normal equations to compute the cost function (theta) for the sale price
# of houses based on square footage and number of bedrooms.

# Load functions and data sets
source("featureNormalize.R")
source("gradientDescent.R")
source("normalEquation.R")
data <- read.csv("~/Kaggle Projects/R Tutorial/ex1data2.txt", header=FALSE)

# Compute theta using a normalized set of features.
num_features <- length(data[1,])
X <- data[,1:(num_features - 1)]
y <- data[,num_features]
m <- length(y)
X_norm <- featureNormalize(X)
X_norm <- cbind(rep(1,m), X_norm)
X_norm <- data.matrix(X_norm)
y <- data.matrix(y)

theta <- c(rep(0,num_features))
theta <- gradientDescent(X_norm, y, theta, 0.01, 400)
test <- featureNormalize(matrix(c(1650,3)))
test <- cbind(rep(1,1),t(test))
test %*% theta

# Compute theta using the normal equation.
X <- cbind(rep(1,m), X)
X <- data.matrix(X)
test <- matrix(c(1650,3))
test <- cbind(rep(1,1),t(test))
theta <- normalEquation(X, y)
test %*% theta

