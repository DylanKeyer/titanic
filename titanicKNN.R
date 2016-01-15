source('C:/Users/dylan_000/OneDrive/R/titanic.R')
titanic()

#KNN#
preds <- c('Pclass','Sex','Age','SibSp',
           'Parch','Fare','Embarked', 'Title')

#make the Pclass var a factor variable to then binarize
train$Pclass <- paste('class',train$Pclass,sep='')
test$Pclass <- paste('class',test$Pclass,sep='')

#binarize for KNN

#train
for(pred in preds){
  if(!(class(train[,pred]) %in% c('numeric','integer'))){
    train <- binarize(train[,pred], train)
  }
}

#test
for(pred in preds){
  if(!(class(test[,pred]) %in% c('numeric','integer'))){
    test <- binarize(test[,pred], test)
  }
}

#save labels for model fitting
labels <- train$Survived

#select only the predictors
trainData <- train %>% select(-Survived, -PassengerId, 
                              -Fare, -Pclass, -Sex, -Ticket,
                              -Cabin, -Embarked, -Title)

testData <- test %>% select(-Survived, -PassengerId, 
                            -Fare, -Pclass, -Sex, -Ticket,
                            -Cabin, -Embarked, -Title)

#set control parameters for repeated cross-validation (3 times)
ctrl <- trainControl(method = 'repeatedcv', repeats = 3, summaryFunction = twoClassSummary, classProbs = TRUE)

#set k to be sqrt of number of rows of training data
k <- round(sqrt(nrow(trainData)),digits=0)

#set a grid of k values to iteratre over and perform cross validation -- best k will be chosen
grid <- expand.grid(k = seq(1, k, by=1))

#fit KNN model
KNNfit <- train(x = trainData, y = labels, method = 'knn',
                trControl = ctrl, 
                tuneGrid = grid)

#predict
preds <- predict(KNNfit, testData)

#create submission df
submission_KNN2 <- data.frame(PassengerId = test$PassengerId,
                             Survived = ifelse(preds == 'Survived',
                                               1,0))
write.csv(file = 'submission_knn2.csv', submission_KNN2, 
          row.names = FALSE)
