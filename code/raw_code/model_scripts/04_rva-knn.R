###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(RPostgreSQL)
library(dplyr)
library(class)
source("../helper_scripts/helper00_project-db-connection.R")

dbGetQuery(spatialdb, "SELECT * 
           FROM pg_catalog.pg_tables 
           WHERE schemaname = 'public'") 

# accuracy function
accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}

#normalize function
normalize <- function(x){
  (x -min(x))/(max(x)-min(x))   
}

# Load training, testing, and validation data setsS
train_set <- dbGetQuery(defaultdb, "SELECT * from training_data")
test_set <- dbGetQuery(defaultdb, "SELECT * from testing_data")
valid_set <- dbGetQuery(defaultdb, "SELECT * from validation_data")

# turn gentrification label into factor
train_set$gent_n_eligible <- as.factor(train_set$gent_n_eligible)
test_set$gent_n_eligible <- as.factor(test_set$gent_n_eligible)
valid_set$gent_n_eligible <- as.factor(valid_set$gent_n_eligible)

# Normalize training data's continous variables
train_norm <- as.data.frame(lapply(train_set[,c(2:18, 20:27)], normalize))

# save gentrification labels from training set
target_cat <- train_set$gent_n_eligible

## Run predictions on test set ##
# Normalize testing data's continous variables
test_norm <- as.data.frame(lapply(test_set[,c(2:18, 20:27)], normalize))

# Fix NA values in testing's normalized data
test_norm <- test_norm %>%
  replace(., is.na(.), 0) 

# Save gentrification labels from testing set
test_cat <- test_set$gent_n_eligible

# Run model for testing
knn_test_pred_list <- knn(train_norm, test_norm, cl = target_cat, k = 10)

# Confusion matrix and accuracy of test results
knn_test_conf_mx <- table(knn_test_pred_list, test_cat)
accuracy(knn_test_conf_mx)


## Run predictions on validation set ##
# Normalize validation data's continous variables
val_norm <- as.data.frame(lapply(valid_set[,c(2:18, 20:27)], normalize))

# Fix NA values in validations's normalized data
val_norm <- val_norm %>%
  replace(., is.na(.), 0) 

# Save gentrification labels from validations set
val_cat <- valid_set$gent_n_eligible

# Run model for validation
knn_val_pred_list <- knn(train_norm, val_norm, cl = target_cat, k = 10)

# Confusion matrix and accuracy of validation results
knn_val_conf_mx <- table(knn_val_pred_list, val_cat)
accuracy(knn_val_conf_mx)


