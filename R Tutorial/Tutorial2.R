# Load data sets
train <- read.csv("~/Kaggle Projects/R Tutorial/train.csv")
test <- read.csv("~/Kaggle Projects/R Tutorial/test.csv")

# View gender data
summary(train$Sex)

# View survival rates of men vs women
prop.table(table(train$Sex, train$Survived),1)

# Submission for survival rate based on gender
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "womensurvive.csv", row.names = FALSE)

# View age data
summary(train$Age)

# Make a column for children
train$Child <- 0
train$Child[train$Age < 18] <- 1
summary(train$Child)

# Experiment with aggregates
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Catagorize fairs
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-30'
train$Fare2[train$Fare < 10] <- '<10'

# Create a new aggregate based on age, sex, and class
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create a new prediction that assumes that women 
# who paid more than $20 for a ticket fared poorly
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "poorwomensurvive.csv", row.names = FALSE)








