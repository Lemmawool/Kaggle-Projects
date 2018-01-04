# The following code uses gradient decent with feature normalization 
# to find a solution for the House Prices:Advanced Regression Techniques
# Kaggle Competition.

# Load functions and data sets
source("featureNormalize.R")
source("gradientDescent.R")
source("normalEquation.R")
train <- read.csv("~/Kaggle Projects/Housing Prices/train.csv", header=TRUE)
test <- read.csv("~/Kaggle Projects/Housing Prices/test.csv", header=TRUE)

# Use fewer features.
selectedFeatures_train <- data.frame(
  "LotArea" = train$LotArea,
  "OverallQual" = train$OverallQual,
  "OverallCond" = train$OverallCond,
  "SF" = train$GrLivArea
)

selectedFeatures_test <- data.frame(
  "LotArea" = train$LotArea,
  "OverallQual" = train$OverallQual,
  "OverallCond" = train$OverallCond,
  "SF" = train$GrLivArea
)

# Convert the rows and columns in train and test to numeric values.
train_matrix <- sapply(selectedFeatures_train, as.numeric)
train_matrix[is.na(selectedFeatures_train )] <- 0
test_matrix <- sapply(selectedFeatures_test, as.numeric)
test_matrix[is.na(selectedFeatures_test)] <- 0

# Compute theta using a normalized set of features.
num_features <- length(train_matrix[1,])
X_train <- train_matrix[,1:(num_features - 1)]
y_train <- train_matrix[,num_features]
m_train <- length(y_train)

X_train_norm <- featureNormalize(X_train)
X_train_norm <- cbind(rep(1,m_train), X_train_norm)

X_test <- test_matrix[,1:(num_features - 1)]
m_test <- length(X_test[,1])
X_test_norm <- featureNormalize(X_test)
X_test_norm <- cbind(rep(1,m_test), X_test_norm)

theta <- c(rep(0,num_features))
theta <- gradientDescent(X_train_norm, y_train, theta, 0.005, 400)

# Compute the SalePrice for the test data and create our submission.
test_costs <- X_test_norm %*% theta
test_costs <- abs(tail(test_costs, length(test_costs) - 1))*125
test_ids <- test$Id
submission <- data.frame(Id = test_ids, SalePrice = test_costs)
write.csv(submission, file = "featureNormGradDesc.csv", quote = FALSE, row.names = FALSE)
# Score of 0.46531



