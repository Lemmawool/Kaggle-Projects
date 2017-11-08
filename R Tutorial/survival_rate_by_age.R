# Load data sets
train <- read.csv("~/Kaggle Projects/R Tutorial/train.csv")
test <- read.csv("~/Kaggle Projects/R Tutorial/test.csv")

summary(train$Age)

summary(train$Age >= 50)

# Make a column for the elderly
train$Elderly <- 0
train$Elderly[train$Age >= 48] <- 1
summary(train$Elderly)

# Experiment with elderly aggregates
aggregate(Survived ~ Elderly, data=train, FUN=sum)
aggregate(Survived ~ Elderly, data=train, FUN=length)
aggregate(Survived ~ Elderly, data=train, FUN=function(x) {sum(x)/length(x)})

aggregate(Survived ~ Elderly + Sex, data=train, FUN=sum)
aggregate(Survived ~ Elderly + Sex, data=train, FUN=length)
aggregate(Survived ~ Elderly + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Looks like old women do quite well, what age group doesn't?

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

# Try making all older women survive. Score is still 0.77990
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
test$Survived[test$Sex == 'female' & test$Age >= 48] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "pooroldwomensurvive.csv", row.names = FALSE)










