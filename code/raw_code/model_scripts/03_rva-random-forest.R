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
library(randomForest)
source("../helper_scripts/helper00_project-db-connection.R")

dbGetQuery(spatialdb, "SELECT * 
                        FROM pg_catalog.pg_tables 
                          WHERE schemaname = 'public'") 

# accuracy function
accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}

# Load training, testing, and validation data setsS
train_set <- dbGetQuery(defaultdb, "SELECT * from training_data")
test_set <- dbGetQuery(defaultdb, "SELECT * from testing_data")
valid_set <- dbGetQuery(defaultdb, "SELECT * from validation_data")

# turn gentrification label into factor
train_set$gent_n_eligible <- as.factor(train_set$gent_n_eligible)
test_set$gent_n_eligible <- as.factor(test_set$gent_n_eligible)
valid_set$gent_n_eligible <- as.factor(valid_set$gent_n_eligible)

# Set seed for reproduction
set.seed(182)

# Fit model using training data
rf_fit <- randomForest(gent_n_eligible ~ built_unbuilt_ratio + Downtown + 
                       Highways + Historic_Urban_Neighborhood + Industrial_Land + 
                       Post_Industrial_Zone + Post_War_Suburb + 
                       Streetcar_Neighborhood + num_props + med_age + iqr_age + 
                       med_stories + iqr_stories + med_finsize + iqr_finsize + 
                       med_rooms + iqr_rooms + pct_r_one_st + pct_r_two_st + 
                       pct_sing_fam + pct_condo_hr + pct_dupl + pct_gable + 
                       pct_wfws + pct_brick, data = train_set, 
                     na.action=na.roughfix)

# View variable importance
var_imp_list <- importance(rf_fit)
var_imp_df <- as.data.frame(var_imp_list)


## Run predictions on test set ##
rf_test_pred_list <- predict(rf_fit, newdata = test_set)

# Confusion matrix and accuracy of test results
rf_test_conf_mx <- table(rf_test_pred_list, test_set$gent_n_eligible)
accuracy(rf_test_conf_mx)


## Run predictions on validation set ##
rf_val_pred_list <- predict(rf_fit, newdata = valid_set)

# Confusion matrix and accuracy of validation results
rf_val_conf_mx <- table(rf_val_pred_list, valid_set$gent_n_eligible)
accuracy(rf_val_conf_mx)


