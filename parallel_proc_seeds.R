### Setting seeds for multiple cores ###

library(caret)
library(tidyverse)

# First, load and partition your data
# Remeber to set a seed before partitioning

# Then set the number of cores for parallel processing
registerDoMC(detectCores())

# Choose hyperparameter grid
grid <- expand.grid(.alpha = seq(0,1,by=0.1), .lambda = seq(0,1,by=0.1))

# Choose specs for trainControl
trControl <- trainControl(method = "repeatedcv", # If method = "cv" and repeats is not specified, it defaults to 1
                          # And if you don't intend repeats to be 1, then change method to = "repeatedcv"
                          number = 5,
                          repeats = 5,
                          seeds = resample_seeds,
                          savePredictions = TRUE,
                          classProbs = T)

# Seeds for repeatedcv/parallel processing
# Length = (n_repeats*nresampling)+1, or (number*repeats)+1
resample_seeds <- vector(mode = "list", length = 26)
# Number of seeds per resampling should be (in theory) the number of tuning parameters tested (cause that's the number of models tested)
# In practice, glmnet trains one model per alpha value and fits all lambda values simultaneously
# So for glmnet, number of seeds per resampling should be the number of alpha values in your grid
# In this case, it's 11
set.seed(2837)
for(i in 1:25) resample_seeds[[i]] <- sample.int(n = 3000, 11)
# Last seed is for the final model (should be a single value, not equal to the number of alphas)
set.seed(1284)
resample_seeds[[26]] <- sample.int(n = 3000, 1)
# List of lists doesn't work, list of vectors does
resample_seeds <- lapply(resample_seeds, as.vector)

# Run model
change_model <- train(y = training_data$outcome_var,
                      x = training_predictors,
                      trControl = trControl,
                      importance = T,
                      method = "glmnet",
                      tuneGrid = grid)