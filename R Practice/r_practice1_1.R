# Functions and data sets
source("computeCost.R")
source("gradientDescent.R")
data <- read.csv("~/Kaggle Projects/R Tutorial/ex1data1.txt", header=FALSE)

X <- data[,1]
y <- data[,2]
m <- length(y)

plot(X,y)

X <- matrix(c(rep(1,m), X), nrow=m, ncol=2)
theta <- c(0,0)
J <- computeCost(X, y, theta)
print(c("With theta = [0 0] the cost coumputed is:", J))

theta <- c(0,0)
theta <- gradientDescent(X, y, theta, 0.01, 1500)
print(c("Value of theta: ", theta))


