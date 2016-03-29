#######################################################
#           Reverse Feature Engineering               #
#                                                     #
#######################################################

### LOADING  ##########################################
train <- read.csv("F:/KaggleProjectsAndData/Santander Customer Satisfaction/data/train.csv")
test  <- read.csv("F:/KaggleProjectsAndData/Santander Customer Satisfaction/data/test.csv")

ncol0 <- ncol(train)
### 1 #################################################
### Removing constant features 
cat("removing constant features\n")
toRemove <- c()
feature.names <- names(train)
for (f in feature.names) {
  if (sd(train[[f]])==0) {
    toRemove <- c(toRemove,f)
    cat(f,"is constant\n")
  }
}
train.names  <- setdiff(names(train), toRemove)
train        <- train[,train.names]
test.names   <- setdiff(names(test), toRemove)
test         <- test[,test.names]
toRemove
cat("-------------------------\n")
### 2 #################################################
# Removing features being a specific linear function 
# of other features where all coefficients are
# positive integer numbers
cat("reverse feature engineering\n")
trainC <- train[which(train$ID %% 50 == 1),] # faster, for first run
# trainC <- train                            # slower, for final run

trainC$ID     <- NULL
trainC$TARGET <- NULL
feature.names <- names(trainC)
toRemove <- c()
for (fn in feature.names) {
  formula0 <- paste0(fn," ~.")
  fit <- lm(formula0, trainC)
  coe <-  ifelse(is.na(fit$coefficients[]),0,fit$coefficients[])
  coeP1 <- ifelse(abs(round(coe[])-coe[]) < 0.001 & coe[]>0, round(coe[]),0)
  coeM1 <- ifelse(abs(round(coe[])-coe[]) < 0.001 & coe[]<0, round(coe[]),0)
  coeP10 <- ifelse(coe[]>0, round(coe[]),0)
  coeM10 <- ifelse(coe[]<0, round(coe[]),0)
  sumP1 <- sum(coeP1)
  sumM1 <- sum(coeM1)
  if (sumP1>1 & sumM1==0 & coeP1==coeP10 & coeM1==coeM10) {
    toRemove <- c(toRemove,fn)
    cat(fn,"is a function of other features (sum of coeff =",sumP1,")\n")
  }
}
train.names  <- setdiff(names(train), toRemove)
train        <- train[,train.names]
test.names   <- setdiff(names(test), toRemove)
test         <- test[,test.names]
toRemove
cat("-------------------------\n")
### 3 #################################################
### Removing highly correlated features
cat("removing highly correlated features\n")
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if ((cor(train[[f1]] , train[[f2]])) > 0.999) {
      cat(f1, "and", f2, "are highly correlated \n")
      toRemove <- c(toRemove, f2)
    }
  }
}
train.names  <- setdiff(names(train), toRemove)
train        <- train[,train.names]
test.names   <- setdiff(names(test), toRemove)
test         <- test[,test.names]
toRemove
cat("-------------------------\n")
####################################################
removed <- ncol0-ncol(train)
cat("\n ",removed," features have been removed\n")
### SAVING #########################################

write.csv(train, "F:/KaggleProjectsAndData/Santander Customer Satisfaction/data/train1.csv", row.names=F, quote=F)
write.csv(test, "F:/KaggleProjectsAndData/Santander Customer Satisfaction/data/test1.csv", row.names=F, quote=F)
