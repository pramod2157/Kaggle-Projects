rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
prints = function(...) { print(paste(...)) }
library(xgboost)
library(Matrix)
library(plotly)


set.seed(1)

cats = function(...) { cat(...); cat("\n") }
prints = function(...) { print(paste(...)) }

train <- read.csv("H:/KaggleProjectsAndData/Santander Customer Satisfaction/data/train.csv")
test  <- read.csv("H:/KaggleProjectsAndData/Santander Customer Satisfaction/data/test.csv")

##### Removing IDs
train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

##### Extracting TARGET
train.y <- train$TARGET
train$TARGET <- NULL

##### 0 count per line
count0 <- function(x) {return( sum(x == 0) )}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)

##### Removing constant features
cats("## Removing the constants features.")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cats(f, "is constant in train. We delete it.")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cats(f1, "and", f2, "are equals.")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names]
test <- test[, feature.names]

train$TARGET <- train.y
train <- sparse.model.matrix(TARGET ~ ., data = train)
dtrain <- xgb.DMatrix(data=train, label=train.y)

test$TARGET <- -1
test <- sparse.model.matrix(TARGET ~ ., data = test)
dtest <- xgb.DMatrix(data=test)

watchlist <- list(train=dtrain, test=test)

param <- list( objective = "binary:logistic", booster = "gbtree",eval_metric = "auc",eta = 0.05, max_depth = 5,subsample = 0.7,colsample_bytree = 0.7 )

Iter = c()
test.auc.mean = c()

for (i in 1:50) {
  set.seed(i)
  xgcv = xgb.cv( params = param, data = dtrain, nround = 750, nthread = 25, nfold = 5, print.every.n = round(1000/5) , early.stop.round = 20, verbose = 1, maximize = T)
  max.test.auc.mean = max(xgcv$test.auc.mean)
  bestIter = match( max(xgcv$test.auc.mean), xgcv$test.auc.mean)
  cats("\nBest Iteration = ",bestIter," ; MAX TEST AUC = ",max.test.auc.mean)
  Iter = rbind(Iter,bestIter)
  test.auc.mean = rbind(test.auc.mean, max.test.auc.mean)
}

p = plot_ly(data = df, y = V1 )
p <- layout(p,  title = "Mean AUC over Iterations", xaxis = list(title = "Runs"), yaxis = list(title = "Mean AUC"))
p

df = as.data.frame(Iter)
p = plot_ly(data = df, y = V1)
df = as.data.frame(test.auc.mean)
p <- layout(p,  title = "Best Iteration over Seeds", xaxis = list(title = "Runs"), yaxis = list(title = "Best Iteration"))
p

