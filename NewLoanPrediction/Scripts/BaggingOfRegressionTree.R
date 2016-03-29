
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
NewTrain_Y<- ifelse(NewTrain_Y == 'Y',1,0)
NewTrain_Y <- data.frame(as.numeric(NewTrain_Y))

NewTrain <- cbind(NewTrain_X, NewTrain_Y)
target <- data.frame(NewTrain_Y)


#-----------------Creating Bag of Models-----------------------

library(tree)
B=100 # number of trees

# Generate k learners

length_divisor<-2

rm(predictions) # remove predictions matrix if exist any

predictions<-cbind(1:nrow(NewTest)) # create predictions matrix to store bagged trees columnwise

#apply bagging

for (b in 1:B)
{
  # Take a bootsrapes sample from the training data
  
  sampledata<-sample(nrow(NewTrain_X),size=floor((nrow(NewTrain_X)/length_divisor)))
  
  
  ### Fit a decision tree on bootstraped sample
  
  NewTrain_Y <- target[sampledata,]
  
  tree_model<-tree(NewTrain_Y ~ .,NewTrain_X[sampledata,][,-1]) # fhat
  
  ## check how the model is doing using the testing dataset
  # Make the prediction
  tree_pred<-data.frame(predict(object=tree_model,NewTest)) # fhat(x)
  #tree_pred
  #Store the predictions for each bootstrapped sample
  predictions<-cbind(predictions,tree_pred)
  
}


predictions <- predictions[,-1] # Remove extra column added 
summary(predictions)

#mean sqaured error for bagged model
yhat<-rowMeans(predictions) ## take mean of rowsi.e.avrage of fhat(x)
summary(yhat)

pred <- ifelse(yhat>0.5, 'Y', 'N')
pred <- factor(pred)
summary(pred)
submission <- data.frame(Loan_ID = NewTest$Loan_ID, Loan_Status = pred)
write.csv(submission, 'F:/KaggleProjectsAndData/AnalyticsVidhya/NewLoanPrediction/Submissions/BaggedRegressionTree.csv', row.names=FALSE, quote = FALSE)
#accuracy  0.777777777778
