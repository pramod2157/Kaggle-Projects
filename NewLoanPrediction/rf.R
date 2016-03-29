library(randomForest)

# data frame preparation
train <- read.csv("C:/Users/coep/Desktop/eNSEMBLE/DATA/NewLoanPredictionAnalyticsVidya/train.csv",header = TRUE)
test <- read.csv("C:/Users/coep/Desktop/eNSEMBLE/DATA/NewLoanPredictionAnalyticsVidya/test.csv",header = TRUE)

#make Y,N as 1,0
train$Loan_Status<- ifelse(train$Loan_Status == 'Y',1,0)
#train$Gender<- ifelse(train$Gender == "",'Male',NA)
summary(train)
train$Gender
#deal with missing values
train[is.na(train$LoanAmount), "LoanAmount"] <- mean(na.omit(train$LoanAmount))
train[is.na(train$Loan_Amount_Term), "Loan_Amount_Term"] <- mean(na.omit(train$Loan_Amount_Term))
train[is.na(train$Credit_History), "Credit_History"] <- median(na.omit(train$Credit_History))

test[is.na(test$LoanAmount), "LoanAmount"] <- mean(na.omit(test$LoanAmount))
test[is.na(test$Loan_Amount_Term), "Loan_Amount_Term"] <- mean(na.omit(test$Loan_Amount_Term))
test[is.na(test$Credit_History), "Credit_History"] <- median(na.omit(test$Credit_History))


 x_train <- train[,-13]
 y_train <- train[,13]
 x <- cbind(x_train,y_train)
 x$y_train <- as.factor(x$y_train)

 str(x)
 str(test)
 summary(x)
 
 rfModel <- randomForest(y_train ~ ., x[,-1] ,  ntree=100)
 
 

 summary(rfModel)
 
 summary(test)
 

 
 fitted.results <- predict(rfModel,test[,-1],type ='class')
 fitted.results <- ifelse(fitted.results==1, 'Y', 'N')
 
 submission <- cbind(test$Loan_ID, fitted.results)


 write.csv(submission,"C:/Users/coep/Desktop/sample1.csv")
 #fitted.results <- ifelse(fitted.results > 0.5,1,0)
 misClasificError <- mean(fitted.results != y_train)
 print(paste('Accuracy',1-misClasificError))
