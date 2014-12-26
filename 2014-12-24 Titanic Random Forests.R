# Based on Tutorial of Trevor Stephens http://trevorstephens.com/
# Finished by Dennis Lyubyvy 

# Set working directory and import datafiles
setwd("~/github/Titanic-Kaggle/")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Install and load required packages for decision trees and forests
library(rpart)
#install.packages('randomForest')
library(randomForest)
#install.packages('party')
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

#Agefit <- cforest(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
#                data=combi[!is.na(combi$Age),], controls=cforest_unbiased(ntree=2000, mtry=3))


combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# Check what else might be missing
summary(combi)
# Fill in Embarked blanks
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
#combi$Embarked[c(62,830)] = "C"
combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))
#combi$Fare[1044] <- median(combi$Fare[which(combi$Pclass==3)], na.rm=TRUE)
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
#log Age (Dennis)
#combi$LogAge <- log(combi$Age)
combi$Sector <- substr(combi$Cabin, 1, 1)
combi$Sector[combi$Sector==''] <- ''
combi$Sector <- as.factor(combi$Sector)

# New factor for Random Forests, only allowed <32 levels, so reduce number
combi$FamilyID2 <- combi$FamilyID
# Convert back to string
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)

combi$Ticket <- as.factor(combi$Ticket)
tickets <- unique(combi$Ticket)
for (t in tickets) {
    who <- which 
    print(t)
    
}

#PCA
str(combi)

combi_pca <- combi
str(combi_pca)
combi$Sex <- as.numeric(combi$Sex)
combi$Sex <- as.numeric(combi$Sex)


pca_data <- prcomp(combi_pca, scale=TRUE)
apply(combi_pca , 2, mean)
str(combi_pca)

summary(pca_data)
plot(pca_data,type="lines")
loadings(pca_data)
pca_data$scores
combi$PCA1 <- pca_data$scores[,1]
combi$PCA2 <- pca_data$scores[,2]
#RANDOM FORESTS

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build Random Forest Ensemble
#set.seed(415)
#fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2 + Age_Pclass,
#                    data=train, importance=TRUE, ntree=2000)
# Look at variable importance
#varImpPlot(fit)
# Now let's make a prediction and write a submission file
#Prediction <- predict(fit, test)
#submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# CV for Random Forest
n <- dim(train)[1]
fold <- {}
train_fold <- {}
fold[1] <- 0
CR <- {}

for (i in 1:5) {
    fold[i+1] <- round(n*i/5)
    train_fold[i] <- list((fold[i]+1):fold[i+1])
    train_fold_data <- train[train_fold[[i]],]

    fit <- cforest(as.factor(Survived) ~ Pclass + 
                       Sex + 
                       Age + 
                       SibSp + 
                       Parch + 
                       Fare + 
                       Embarked + 
                       Title + 
                       FamilySize + 
                       FamilyID +
                       Sector 
#                      + Ticket
#                      + PCA1
#                       PCA2
,data = train_fold_data, controls=cforest_unbiased(ntree=2000, mtry=3))

train_pred <- predict(fit, train_fold_data, OOB=TRUE, type = "response")
CR[i] <- sum(train_pred==train_fold_data$Survived)/length(train_pred)

}

cat('Correctness rate:',mean(CR))

#build train tree and test prediction 
fit <- cforest(as.factor(Survived) ~ Pclass + 
                   Sex + 
                   Age + 
                   SibSp + 
                   Parch + 
                   Fare + 
                   Embarked + 
                   Title + 
                   FamilySize + 
                   FamilyID +
                   Sector 
               #+
                   #PCA1 +
                   #PCA2
               ,data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "forest.csv", row.names = FALSE)


# CHECK


