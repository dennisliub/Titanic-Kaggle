# Trevor Stephens - 18 Jan 2014
# Titanic: Getting Started With R - Part 5: Random Forests
# Full guide available at http://trevorstephens.com/

# Set working directory and import datafiles
setwd("~/Desktop/Titanic Kaggle/")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Install and load required packages for decision trees and forests
library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Fill in Age NAs
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age),], method="anova")

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
# Check what else might be missing
summary(combi)
# Fill in Embarked blanks
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#log Age (Dennis)
#combi$LogAge <- log(combi$Age)
combi$Sector <- substr(combi$Cabin, 1, 1)
combi$Sector[combi$Sector==''] <- 'Z'

# New factor for Random Forests, only allowed <32 levels, so reduce number
combi$FamilyID2 <- combi$FamilyID
# Convert back to string
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)

#SVM

install.packages('e1071')
library(e1071)


combi_svm <- as.data.frame(combi[,c('Pclass','Sex','Age','SibSp','Parch','Fare','Embarked','Title','FamilySize','FamilyID','Survived')])
colnames(combi_svm) <- c('Pclass','Sex','Age','SibSp','Parch','Fare','Embarked','Title','FamilySize','FamilyID','Survived')
combi_svm_train <- combi_svm[1:891,]
combi_svm_test <- combi_svm[892:1309,]

#tuning out our SVM with 10 k-fold cross-validation
#tune.out=tune(svm,Survived~., data=combi_svm_train, kernel="radial", 
#              ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4) ))
tune.out <- tune(svm,Survived~., data=combi_svm_train, kernel="radial", 
              ranges=list(cost=c(0.5,1,3,5),gamma=c(0.5,1,2,3,4) ))

summary(tune.out)
#   best parameters:
#   cost gamma
#   1   0.5

pred <- predict(tune.out$best.model, newx=combi_svm_test)
submit <- data.frame(PassengerId = combi_svm_test$PassengerId, Survived = pred)
write.csv(submit, file = "svm.csv", row.names = FALSE)



