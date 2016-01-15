
titanic <- function(){
  #Titanic gbm
  library(caret)
  library(ggplot2)
  library(dplyr)
  library(gbm)
  library(stringr)
  library(pROC)
source('C:/Users/dylan_000/OneDrive/R/binarize.R')
setwd('C:/Users/dylan_000/OneDrive/KaggleData/titanic')

#load training and testing data
train <- read.csv('train.csv', stringsAsFactors = FALSE)
test <- read.csv('test.csv', stringsAsFactors = FALSE)

#deal with the missing vals for Embarked using mode
temp <- table(as.vector(train$Embarked))

train$Embarked[train$Embarked == ''] <- names(temp)[temp == max(temp)]

#deal with missing Fare in test data
test$Fare[is.na(test$Fare)] <- mean(test$Fare,na.rm=TRUE)

#helper fxn to generate new feature (title)
title <- function(x){
  return(strsplit(x = x, split = '[,.]')[[1]][2])
}

#create title var
train$Title <- sapply(train$Name, FUN = function(x){gsub(x = title(x), pattern = ' ', rep = '')})
train$Name <- NULL

#create title var
test$Title <- sapply(test$Name, FUN = function(x){gsub(x = title(x), pattern = ' ', rep = '')})
test$Name <- NULL

test$Survived <- NA

combined <- rbind(train,test)

#create a training dataset of rows with an age value to use for imputation, and separate out rows with NA for age
noNA <- combined[!(is.na(combined$Age)),]
withNA <- combined[is.na(combined$Age),]

#tune gbm model to pred age
noNA2 <- noNA %>% select(-Survived, -Cabin, -PassengerId, -Ticket)
ageFit <- train(Age ~., data = noNA2, method = 'gbm', verbose = FALSE)

#predict age based on gbm model fit for age
withNA$Age <- round(predict(ageFit, withNA), 0)

#bind back together the training data
temp <- rbind(noNA, withNA)
train <- temp[!(is.na(temp$Survived)),]
test <- temp[is.na(temp$Survived),]

#convert age to integer
train$Age <- as.integer(train$Age)

#get values of all possible titles 
titleLevels <- unique(c(unique(train$Title),unique(test$Title)))

train$Title <- factor(train$Title, levels = titleLevels)
test$Title <- factor(test$Title, levels = titleLevels)

#convert label to factor with 2/ levels
train$Survived <- factor(train$Survived, levels = c(1,0), labels = c('Survived', 'Died'))

}
