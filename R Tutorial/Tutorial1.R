# Set working directory
setwd("~/Kaggle Projects/R Tutorial")

#Import datafiles
train <- read.csv("~/Kaggle Projects/R Tutorial/train.csv")
test <- read.csv("~/Kaggle Projects/R Tutorial/test.csv")

# View dataframes
str(train)
str(test)

# Isolate a column
train$Survived

# Basic statistics
table(train$Survived)

# Percentage of died vs survived:
prop.table(table(train$Survived))

# Create a survived column in test
test$Survived <- rep(0,418)

# Create a csv for submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)



