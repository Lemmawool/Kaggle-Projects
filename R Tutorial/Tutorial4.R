# Load data sets
train <- read.csv("~/Kaggle Projects/R Tutorial/train.csv")
test <- read.csv("~/Kaggle Projects/R Tutorial/test.csv")

# View a passenger name at a specific index
train$Name[1]

# Merge test and train into a new dataframe
test$Survived <- NA
combi <- rbind(train, test)

# Convert the name column from factors to strings
combi$Name <- as.character(combi$Name)
combi$Name[1]

# Split a string
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Create a new column for title
# Then remove the excess spaces
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)

# Combine Mademoiselle & Madame
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

# Consolidate passengers with rare titles
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

# Create a new column for family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Create a sirname & family id column
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

# Refine family size
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

# Create a dataframe containing FamilyID and appearence frequency
famIDs <- data.frame(table(combi$FamilyID))

# Filter out FamilyIDs that appear less than twice
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# Break the combined dataset back into train and test
train <- combi[1:891,]
test <- combi[892:1309,]

# Create a decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train,
             method="class")

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

# Make a new prediction based on a decision tree with factors
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "factordtree.csv", row.names = FALSE)









