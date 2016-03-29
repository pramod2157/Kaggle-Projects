
setwd("F:/KaggleProjectsAndData/AnalyticsVidhya/NewLoanPrediction") # Set the working Directory 
train<-read.csv("Data/train.csv") # Import train Data 
test<-read.csv("Data/test.csv") # Import Test data 

summary(train)
#table(train$Gender,train$Loan_Status)
#table(train$Dependents,train$Loan_Status)
summary(test)

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

NewTrain <- cbind(NewTrain_X, NewTrain_Y)

#----------Build a  Random Forest Model---------------------------


library(randomForest)
rfModel <- randomForest(NewTrain_Y ~ LoanAmount+CoapplicantIncome+ApplicantIncome+Credit_History, NewTrain[,-1] ,  ntree=500,  type = 'classification',importance=TRUE)
VI_F <- importance(rfModel)
VI_F
rfModel
pred <- predict(rfModel,NewTest[,-1],type = )#"class")
pred
pred <- ifelse(pred>1.5, 'Y', 'N')
submission <- data.frame(Loan_ID = NewTest$Loan_ID, Loan_Status = pred)
write.csv(submission, 'F:/KaggleProjectsAndData/AnalyticsVidhya/NewLoanPrediction/Submissions/RF_500.csv', row.names=FALSE, quote = FALSE)
#Accuracy  0.791666666667 as regression
#0.784722222222 as class
