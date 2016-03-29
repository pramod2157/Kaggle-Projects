#### SUPER LEARNER: XGBoost + Random Feature Subset Selection
##
## This script uses L xgboost models fitted from L 
## random subsets of the original features to generate 
## a NxL "level-one" data. The new dataset can be used
## to train a simple GLM model and get test AUC > 0.84
##
## Super Learner reference: 
## http://learn.h2o.ai/content/tutorials/ensembles-stacking/index.html
##
####

### Load packages
library('readr')
library('magrittr')
library('Matrix')
library('xgboost')

### Load data
dt.train <- readr::read_csv('F:/KaggleProjectsAndData/Santander Customer Satisfaction/data/train.csv', 
                            col_types = paste(rep('d', 371), collapse = ''))
dt.test <- readr::read_csv('F:/KaggleProjectsAndData/Santander Customer Satisfaction/data/test.csv', 
                           col_types = paste(rep('d', 370), collapse = ''))

### Convert target to Int
label.name <- 'TARGET'
y <- as.integer(dt.train[[label.name]])

### Sample weights to "balance" classes (use the inverse of class frequency)
class.freq <- table(y) %>% prop.table
row.wt <- ifelse(y == 0, 1/class.freq[1], 1/class.freq[2])

### Remove features with less than 10% of non-zero entries
col.names <- names(dt.train) %>% setdiff(c('ID', label.name))
zero.rate <- sapply(dt.train[col.names], function(dt.col) {
  sum(dt.col == 0)/length(dt.col)
})
keep.cols <- col.names[zero.rate < 0.9]

### Set XGBoost parameters
xgb.params <- list(
  "booster" = "gbtree",
  "eta" = 1e-2,
  "max_depth" = 4,
  "subsample" = 0.7,
  "colsample_bytree" = 0.7,
  "min_child_weight" = 1,
  "objective" = "binary:logistic",
  "eval_metric" = "auc",
  "silent" = 1,
  "nthread" = 4
)

### Train base learners (Note that some models achieve a really great test performance: ~ 0.84)
n.models   <- 5  # ensemble size / number of base learners (10 is a better choice)
n.features <- 50 # random subset size (50 is a good choice but will take longer to train)
n.folds    <- 3  # more CV folds should increase performance (try 5 or 10)
model.perf <- numeric(n.models)
meta.tr    <- vector('list', n.models)
meta.te    <- vector('list', n.models)
for (i in 1:n.models) {
  cat(paste('\n### Model', i, '###\n'))
  
  ## Sample features
  sel.cols <- sample(keep.cols, n.features)
  x.tr <- Matrix(as.matrix(dt.train[sel.cols]), sparse = TRUE)
  dtrain <- xgb.DMatrix(x.tr, label = y, weight = row.wt)
  x.te <- Matrix(as.matrix(dt.test[sel.cols]), sparse = TRUE)
  dtest <- xgb.DMatrix(x.te)
  
  ## Generate level-one data: k-fold CV with early stopping
  cv.out <- xgb.cv(params = xgb.params, data = dtrain, nrounds = 1500, 
                   nfold = n.folds, prediction = TRUE, stratified = TRUE, 
                   verbose = FALSE, early.stop.round = 15, maximize = TRUE)
  model.perf[i] <- max(cv.out$dt$test.auc.mean)
  best.iter <- which.max(cv.out$dt$test.auc.mean)
  meta.tr[[i]] <- cv.out$pred
  
  ## Train base learner
  xgb.model <- xgb.train(data = dtrain, params = xgb.params, 
                         nrounds = best.iter);
  
  ## Generate test data
  meta.te[[i]] <- predict(xgb.model, dtest)
  
  cat(paste('\nAUC:', model.perf[i]))
}

### Save data: use "meta_train.csv" to train your model and "meta_test.csv" to
### generate your predictions
model.names <- paste('Model', 1:n.models, sep = '')
# New traning data
names(meta.tr) <- model.names
meta.tr$Id <- as.integer(dt.train$ID)
meta.tr$Wt <- row.wt
meta.tr$Target <- y
write.csv(meta.tr, 'meta_train.csv', row.names = FALSE, quote = FALSE)
# New test data
names(meta.te) <- model.names
meta.te$Id <- as.integer(dt.test$ID)
write.csv(meta.te, 'meta_test.csv', row.names = FALSE, quote = FALSE)

### ==> Now use Logistic Regression to learn the best weights to combine the base
### learners

### EXAMPLE

 library(caret)
 dt.train <- read.csv('meta_train.csv')
 dt.train$Target <- as.factor(dt.train$Target)
 tc <- trainControl("cv", 5, savePredictions = FALSE, classProbs = TRUE, summaryFunction = twoClassSummary)
 fit <- train(Target ~ Model1 + Model2 + Model3 + Model4 + Model5,
              data = dt.train, method = "glm", trControl = tc,
              family = binomial, metric = 'ROC', trace = FALSE)
 fit$results$ROC