# Load data sets
train <- read.csv("~/Kaggle Projects/R Tutorial/train.csv")
test <- read.csv("~/Kaggle Projects/R Tutorial/test.csv")

# View survival rates by class
prop.table(table(train$Pclass, train$Survived),1)

# Assume only the poor survive. Score of 0.41148,
test$Survived <- 0
test$Survived[test$Pclass == 3] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "poorsurvive.csv", row.names = FALSE)

# Assume only middle class survives. Score of 0.55980.
test$Survived <- 0
test$Survived[test$Pclass == 2] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "middlesurvive.csv", row.names = FALSE)
# 

# Assume only richest survive. Score of 0.65550.
test$Survived <- 0
test$Survived[test$Pclass == 1] <- 1
submit <- data.frame(passengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, "richestsurvive.csv", row.names = FALSE)
# Score of 0.65550

# Assume lowest two classes survive. Score of 0.34449
test$Survived <- 0
test$Survived[test$Pclass >= 2] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "lowest_two_survive.csv", row.names = FALSE)

# Assume upper two classes survive. Score of 0.58851
test$Survived <- 0
test$Survived[test$Pclass <= 2] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, "highest_two.csv", row.names = FALSE)

