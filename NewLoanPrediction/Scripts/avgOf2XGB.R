
output1 <- read.csv("F:/KaggleProjectsAndData/AnalyticsVidhya/NewLoanPrediction/Submissions/xgboost1.csv")
output2 <- read.csv("F:/KaggleProjectsAndData/AnalyticsVidhya/NewLoanPrediction/Submissions/xgboost2.csv")
output3 <- read.csv("F:/KaggleProjectsAndData/AnalyticsVidhya/NewLoanPrediction/Submissions/xgboost3.csv")

summary(output1$Loan_Status)
summary(output2$Loan_Status)
summary(output3$Loan_Status)

finalResult <- (output1$Loan_Status + output2$Loan_Status + output3$Loan_Status)/3

finalResult <- ifelse(finalResult>0.50, 'Y', 'N')

finalResult <- as.factor(finalResult)

submission <- data.frame(Loan_ID=output2$Loan_ID, Loan_Status=finalResult)
cat("saving the submission file\n")

write.csv(submission, "F:/KaggleProjectsAndData/AnalyticsVidhya/NewLoanPrediction/Submissions/Avgxgboost1_3.csv", row.names = F)
