# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(readr) # CSV file I/O, e.g. the read_csv function
library(xgboost)

# Reading the data
dat_train <- read.csv("H:/KaggleProjectsAndData/Santander Customer Satisfaction/data/train.csv", stringsAsFactors = F)
dat_test <- read.csv("H:/KaggleProjectsAndData/Santander Customer Satisfaction/data/test.csv", stringsAsFactors = F)

# Mergin the test and train data
dat_test$TARGET <- NA
all_dat <- rbind(dat_train, dat_test)

# Removing the constant variables
train_names <- names(dat_train)[-1]
for (i in train_names)
{
  if (class(all_dat[[i]]) == "integer") 
  {
    u <- unique(all_dat[[i]])
    if (length(u) == 1) 
    {
      all_dat[[i]] <- NULL
    } 
  }
}

#Removing duplicate columns
train_names <- names(all_dat)[-1]
fac <- data.frame(fac = integer())    

for(i in 1:length(train_names))
{
  if(i != length(train_names))
  {
    for (k in (i+1):length(train_names)) 
    {
      if(identical(all_dat[,i], all_dat[,k]) == TRUE) 
      {
        fac <- rbind(fac, data.frame(fac = k))
      }
    }
  }
}
same <- unique(fac$fac)
all_dat <- all_dat[,-same]

#Removing hghly correlated variables
cor_v<-abs(cor(all_dat))
diag(cor_v)<-0
cor_v[upper.tri(cor_v)] <- 0
cor_f <- as.data.frame(which(cor_v > 0.85, arr.ind = T))
all_dat <- all_dat[,-unique(cor_f$row)]

# Splitting the data for model
train <- all_dat[1:nrow(dat_train), ]
test <- all_dat[-(1:nrow(dat_train)), ]


#Building the model
set.seed(78)
param <- list("objective" = "binary:logistic",booster = "gbtree",
              "eval_metric" = "auc",colsample_bytree = 0.85, subsample = 0.95)

y <- as.numeric(train$TARGET)

#AUC was highest in 310th round during cross validation
xgbmodel <- xgboost(data = as.matrix(train[,-c(1,151)]), params = param,
                    nrounds = 310, max.depth = 4, eta = 0.03,
                    label = y, maximize = T)

#Prediction
res <- predict(xgbmodel, newdata = data.matrix(test[,-c(1,151)]))
res <- data.frame(ID = test$ID, TARGET = res)

write.csv(res, "H:/KaggleProjectsAndData/Santander Customer Satisfaction/submission10.csv", row.names = FALSE)