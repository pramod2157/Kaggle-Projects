library(caret)
library(ggplot2)
data <- read.csv("D:\\Kaggle\\Santander Customer Satisfaction\\data\\train.csv",header = TRUE)
x_test <- read.csv("D:\\Kaggle\\Santander Customer Satisfaction\\data\\test.csv",header = TRUE)
x_train <- data[,-371]
y_train <- data[,371]
x <- cbind(x_train,y_train)
lapply(y_train, class)
summary(data)
head(data)
#training<-sample(nrow(data),3*nrow(data)/4)
#dataTraining <- data[training,]
#dataTest<- data[-training,]
#x_train <- dataTraining[,-25]
#y_train <- dataTraining[,25]
#x_test <- dataTest[,-25]
#x <- cbind(x_train,y_train)
#lapply(x, class)
x$y_train <- as.factor(x$y_train)
#head(x$y_train)
fitcontrol <- trainControl(method = "repeatedcv" , number = 2 ,repeats = 20)

GBAmodel <- train(y_train ~ ., data= x , method = "gbm" , trControl = fitcontrol, verbose= FALSE)
qplot(GBAmodel)
summary(GBAmodel)

fitted.results_gba <- predict(GBAmodel,x_train)
#fitted.results_svm <- ifelse(fitted.results_svm > 0.5,1,0)
misClasificError <- mean(fitted.results_gba != y_train)
print(paste('Accuracy',1-misClasificError))