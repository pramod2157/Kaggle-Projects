library(randomForest)

data <- read.csv("D:\\Kaggle\\Santander Customer Satisfaction\\data\\train.csv",header = TRUE)
x_test <- read.csv("D:\\Kaggle\\Santander Customer Satisfaction\\data\\test.csv",header = TRUE)
x_train <- data[,-371]
y_train <- data[,371]
x <- cbind(x_train,y_train)

x$y_train <- as.factor(x$y_train)
train$TARGET<- as.factor(train$TARGET)
rfModel <- randomForest(y_train ~ ., x ,  ntree=500)
logistic <- glm(y_train ~ ., data=x, family = 'binomial')
fitted.results <- predict(logistic,x_test,type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
submission <- data.frame(ID = x_test$ID, TARGET = fitted.results)
lapply(train, class)
summary(train$TARGET)
table(fitted.results)
fitted.results_rf <- predict(rfModel,x_test)

submission <- data.frame(ID = x_test$ID, TARGET = fitted.results_rf)
write.csv(submission, 'D:\\Kaggle\\Santander Customer Satisfaction\\submission\\logistic_first_simple.csv', row.names=FALSE, quote = FALSE)

summary(fitted.results_rf)

#####################################################

library(Matrix)
library(xgboost)

# ---------------------------------------------------
# Load
orig.train <- read.csv("D:\\Kaggle\\Santander Customer Satisfaction\\data\\train.csv", stringsAsFactors = F)
orig.test <- read.csv("D:\\Kaggle\\Santander Customer Satisfaction\\data\\test.csv", stringsAsFactors = F)
sample.submission <- read.csv("D:\\Kaggle\\Santander Customer Satisfaction\\data\\sample_submission.csv", stringsAsFactors = F)

orig.test$TARGET <- -1
merged <- rbind(orig.train, orig.test)


# ---------------------------------------------------
# Convert
feature.train.names <- names(orig.train)[-1]
for (f in feature.train.names) {
  if (class(merged[[f]]) == "numeric") {
    merged[[f]] <- merged[[f]] / max(merged[[f]])
  } else if (class(merged[[f]]) == "integer") {
    u <- unique(merged[[f]])
    if (length(u) == 1) {
      merged[[f]] <- NULL
    } else if (length(u) < 200) {
      merged[[f]] <- factor(merged[[f]])
    }
  }
}

# ---------------------------------------------------
# Split
train <- merged[merged$TARGET != -1, ]
test <- merged[merged$TARGET == -1, ]

# ---------------------------------------------------
# Features
feature.names <- names(train)
feature.names <- feature.names[-grep('^ID$', feature.names)]
feature.names <- feature.names[-grep('^TARGET$', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

# ---------------------------------------------------
# Matrix
indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.85))

data <- sparse.model.matrix(feature.formula, data = train[indexes, ])
sparseMatrixColNamesTrain <- colnames(data)
dtrain <- xgb.DMatrix(data, label = train[indexes, 'TARGET'])
rm(data)
dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = train[-indexes, ]),
                      label = train[-indexes, 'TARGET'])
dtest <- sparse.model.matrix(feature.formula, data = test)

watchlist <- list(valid = dvalid, train = dtrain)


# ---------------------------------------------------
# XGBOOST
params <- list(booster = "gbtree", objective = "binary:logistic",
               max_depth = 10, eta = 0.1,
               colsample_bytree = 0.65, subsample = 0.95)
model <- xgb.train(params = params, data = dtrain,
                   nrounds = 5700, early.stop.round = 30,
                   eval_metric = 'auc', maximize = T,
                   watchlist = watchlist, print.every.n = 10)
train <- train[,-3]
rfModel <- randomForest(TARGET ~ ., train ,  ntree=500)
pred <- predict(model, dtest)

# ---------------------------------------------------
# SAVE
submission <- data.frame(ID = test$ID, TARGET = pred)
write.csv(submission, 'xgboost_first_simple.csv', row.names=FALSE, quote = FALSE)


summary(orig.train)
is.na(orig.train)
