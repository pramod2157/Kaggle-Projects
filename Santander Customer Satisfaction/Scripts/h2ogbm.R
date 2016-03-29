# --------------------------------------------------------------------------------------
# Create and connect to a H2O cluster locally
# --------------------------------------------------------------------------------------

# Note that H2O on Kaggle's docker is not the latest stable release
# Download the latest release from http://www.h2o.ai/download/h2o/r
# for new features including random grid search and early stopping 
library(h2o) 
h2o.init(nthreads = -1)

# --------------------------------------------------------------------------------------
# Data
# --------------------------------------------------------------------------------------

# Import CSVs
d_train <- h2o.importFile("../input/train.csv")
d_test <- h2o.importFile("../input/test.csv")
d_sub <- h2o.importFile("../input/sample_submission.csv")

# Reformat num to factor
d_train$TARGET <- as.factor(d_train$TARGET)

# Define features and target
features <- colnames(d_test)[-1]
target <- "TARGET"

# Split frame (for training/validation)
d_split <- h2o.splitFrame(d_train, ratios = 0.95, seed = 1234)

# --------------------------------------------------------------------------------------
# Train a GBM model
# --------------------------------------------------------------------------------------

model <- h2o.gbm(
  # Data
  x = features,
  y = target,
  training_frame = d_split[[1]],
  validation_frame = d_split[[2]],
  
  # Core parameters for GBM
  ntrees = 50,
  learn_rate = 0.01,
  max_depth = 8,
  sample_rate = 0.333,
  col_sample_rate = 0.333,
  balance_classes = TRUE,
  
  # Parameters for early stopping
  # Functions available in latest H2O release (http://www.h2o.ai/download/h2o/r)
  # score_tree_interval = 1,
  # stopping_metric = 'AUC',
  # stopping_rounds = 10,
  # stopping_tolerance = 1e-6
)
summary(model)

# --------------------------------------------------------------------------------------
# Output
# --------------------------------------------------------------------------------------

yhat_test <- as.data.frame(h2o.predict(model, d_test))
d_sub <- as.data.frame(d_sub)
d_sub$TARGET <- round(as.numeric(yhat_test$p1), 6)
write.csv(d_sub, "H2O_GBM_Starter.csv", row.names = FALSE)