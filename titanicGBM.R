source('C:/Users/dylan_000/OneDrive/R/titanic.R')
titanic()
#GBM
##MODELING##

#set control parameters for repeated cross-validation (3 times)
ctrl <- trainControl(method = 'repeatedcv', repeats = 3, summaryFunction = twoClassSummary, classProbs = TRUE)

#set parameters to iterate through during model tuning
grid <- expand.grid(interaction.depth = seq(1, 7, by = 2), 
                    n.trees = seq(100, 1500, by = 100), 
                    shrinkage = c(0.01, 0.10),
                    n.minobsinnode = 1)

#fit GBM tree
gbmFit <- train(data=train, 
                Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
                method = 'gbm', 
                metric = 'ROC', 
                trControl = ctrl,
                tuneGrid = grid,
                verbose = FALSE)


test$Survived <- predict(gbmFit, test)

submission <- data.frame(PassengerId = test$PassengerId, Survived = ifelse(test$Survived == 'Survived',1,0))
View(submission)

write.csv(file = 'submission_gbm.csv', submission, row.names = FALSE)
