# Install and load the packages we ill need
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Load data sets
train <- read.csv("~/Kaggle Projects/R Tutorial/train.csv")
test <- read.csv("~/Kaggle Projects/R Tutorial/test.csv")

# Use rpart and view its output
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
plot(fit)
text(fit)


# Plot fit data using the new libraries
fancyRpartPlot(fit)

# Make a new prediction based on the decision tree
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# Override some of the defaults in rpart
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

# Make a new prediction based on the overridden defaults
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myseconddtree.csv", row.names = FALSE)





