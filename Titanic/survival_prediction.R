# Antempt to predict survivors of the Titanic using logistic regression.
source("titanic_helper_functions.R")

train <- read.csv('~/Kaggle Projects/Titanic/train.csv', header=TRUE)
X_train <- train
X_train$SexNum[X_train$Sex == 'male'] <- 1
X_train$SexNum[X_train$Sex == 'female'] <- 0
selectedFeatures_train <- data.frame(
  "Pclass" = X_train$Pclass,
  "Sex" = X_train$SexNum,
  #"Age" = X_train$Age,
  "SibSp" = X_train$SibSp,
  "Parch" = X_train$Parch,
  "Fare" = X_train$Fare
)
X_train <- as.matrix(selectedFeatures_train)
y_train <- as.matrix(train[,2])

theta <- matrix(c(rep(0,1)), nrow = 5, ncol = 1)
result <- optim(theta, costFuncReg, gradFuncReg, X=X_train, y=y_train, lambda=1, method="BFGS", control=list(maxit=2000))
theta <- result$par

test <- read.csv('~/Kaggle Projects/Titanic/test.csv', header=TRUE)
X_test <- test
X_test$SexNum[X_test$Sex == 'male'] <- 1
X_test$SexNum[X_test$Sex == 'female'] <- 0
selectedFeatures_test <- data.frame(
  "Pclass" = X_test$Pclass,
  "Sex" = X_test$SexNum,
  #"Age" = X_test$Age,
  "SibSp" = X_test$SibSp,
  "Parch" = X_test$Parch,
  "Fare" = X_test$Fare
)
X_test <- as.matrix(selectedFeatures_test)

test$Survived <- predict(X_test, theta)
test$PredNum[test$Survived == TRUE] <- 1
test$PredNum[test$Survived == FALSE] <- 0
test$PredNum[is.na(test$PredNum)] <- 0

submission <- data.frame(PassengerId = test$PassengerId, Survived = test$PredNum)
write.csv(submission, file = "linregtitanic.csv", row.names = FALSE)
# Score of 0.76555





