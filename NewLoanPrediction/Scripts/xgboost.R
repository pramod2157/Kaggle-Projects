
setwd("F:/KaggleProjectsAndData/AnalyticsVidhya/NewLoanPrediction") # Set the working Directory 
train<-read.csv("Data/train.csv") # Import train Data 
test<-read.csv("Data/test.csv") # Import Test data 

set.seed(123)
#summary(train)
#table(train$Gender,train$Loan_Status)
#table(train$Dependents,train$Loan_Status)
#summary(test)

train$Married[train$Married==""] <- "Yes" # In train married is having 3 levels and test 2 levels. 
#So by marginal distribution replacing with yes

test$Loan_Status <- "Test"
merged <- rbind(train,test)

merged$Gender <- as.numeric(merged$Gender)
merged$Dependents <- as.numeric(merged$Dependents)
merged$Married <- as.numeric(merged$Married)
merged$Education <- as.numeric(merged$Education)
merged$Self_Employed <- as.numeric(merged$Self_Employed)
merged$Property_Area <- as.numeric(merged$Property_Area)

NewTrain <- merged[merged$Loan_Status != "Test", ]
NewTest <- merged[merged$Loan_Status == "Test", ]



summary(NewTrain)
summary(NewTest)
NewTest <- NewTest[,-13]
NewTrain_X <- NewTrain[,-13]
NewTrain_Y <- NewTrain[,13]
#-----------Treatment Of Missing Values of train-----------

for(i in 2:ncol(NewTrain_X)){
  NewTrain_X[is.na(NewTrain_X[,i]), i] <- median(NewTrain_X[,i], na.rm = TRUE)
}

#-----------Treatment Of Missing Values of test-----------

for(i in 2:ncol(NewTest)){
  NewTest[is.na(NewTest[,i]), i] <- median(NewTest[,i], na.rm = TRUE)
}

NewTrain_Y <- factor(NewTrain_Y)
#NewTrain_Y <- as.numeric(NewTrain_Y)
NewTrain_Y<- ifelse(NewTrain_Y == 'Y',1,0)
NewTrain_Y <- as.numeric(NewTrain_Y)

NewTrain <- cbind(NewTrain_X, NewTrain_Y)
#-------------------------------------------------------
library(Matrix)
library(xgboost)

train <- sparse.model.matrix(NewTrain_Y ~ ., data = NewTrain[,-1])

dtrain <- xgb.DMatrix(data=train, label=NewTrain_Y)
watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.02,
                max_depth           = 5,
                subsample           = 0.9,
                colsample_bytree    = 0.95
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 560, 
                    verbose             = 2,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

NewTest$NewTrain_Y <- -1
test <- sparse.model.matrix(NewTrain_Y ~ ., data = NewTest[,-1])

preds <- predict(clf, test)
#preds
#preds <- ifelse(preds>0.50, 'Y', 'N')
#preds <- as.factor(preds)
submission <- data.frame(Loan_ID=NewTest$Loan_ID, Loan_Status=preds)
cat("saving the submission file\n")
write.csv(submission, "F:/KaggleProjectsAndData/AnalyticsVidhya/NewLoanPrediction/Submissions/xgboost3.csv", row.names = F)
# 0.777777777778 as factor output
# 0.791666666667 as a regression
#dim(preds)
#preds
