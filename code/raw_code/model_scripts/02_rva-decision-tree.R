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
library(rpart)
library(rpart.plot)
source("../helper_scripts/helper00_project-db-connection.R")

dbGetQuery(spatialdb, "SELECT * 
                        FROM pg_catalog.pg_tables 
                          WHERE schemaname = 'public'") 


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

# Fit model using training data
tree_fit <- rpart(gent_n_eligible ~ built_unbuilt_ratio + Downtown + Highways + 
               Historic_Urban_Neighborhood + Industrial_Land + 
               Post_Industrial_Zone + Post_War_Suburb + Streetcar_Neighborhood +
               num_props + med_age + iqr_age + med_stories + iqr_stories + 
               med_finsize + iqr_finsize + med_rooms + iqr_rooms + 
               pct_r_one_st + pct_r_two_st + pct_sing_fam + pct_condo_hr + 
               pct_dupl + pct_gable + pct_wfws + pct_brick, data = train_set, 
             method = "class")

# Plot out the decision tree
rpart.plot(tree_fit)

## Run predictions on test set ##
# Remove gentrification labels from test set
test_set2 <- test_set %>%
  select(-gent_n_eligible)

# Run prediction on test set
pred1_test <- predict(tree_fit, newdata = test_set2)

# turn predicted results into data frame
pred2_test <- as.data.frame(pred1_test)
names(pred2_test) <- c("e_dng", "gent", "nefg")
pred_df <- test_set %>%
  select(Name, gent_n_eligible) %>%
  bind_cols(pred2_test) %>%
  mutate(
    pred_status = case_when(e_dng > gent & e_dng > nefg ~ 
                              "Eligible, Did Not Gentrify",
                            gent > e_dng & gent > nefg ~ "Gentrified",
                            nefg > e_dng & nefg > gent ~ 
                              "Not Eligible for Gentification")
  ) %>%
  mutate(
    true_pos = case_when(gent_n_eligible == pred_status ~ 1, TRUE ~ 0),
    true_neg = case_when(gent_n_eligible != pred_status ~ 1, TRUE ~ 0)
  )

# Confusion matrix and accuracy of test results
tab1_test <- table(pred_df$pred_status, test_set$gent_n_eligible)
accuracy(tab1_test)

## Run predictions on validation set ##
# Run predictions
pred1_val <- predict(tree_fit, newdata = valid_set)

# turn results into a data frame
pred2_val <- as.data.frame(pred1_val)
names(pred2_val) <- c("e_dng", "gent", "nefg")
pred_df_val <- valid_set %>%
  select(Name, gent_n_eligible) %>%
  bind_cols(pred2_test) %>%
  mutate(
    pred_status = case_when(e_dng > gent & e_dng > nefg ~ 
                              "Eligible, Did Not Gentrify",
                            gent > e_dng & gent > nefg ~ "Gentrified",
                            nefg > e_dng & nefg > gent ~ 
                              "Not Eligible for Gentification")
  ) %>%
  mutate(
    true_pos = case_when(gent_n_eligible == pred_status ~ 1, TRUE ~ 0),
    true_neg = case_when(gent_n_eligible != pred_status ~ 1, TRUE ~ 0)
  )

# Confusion matrix and accuracy of validation results
tab1_val <- table(pred_df_val$pred_status, valid_set$gent_n_eligible)
accuracy(tab1_val)


# roc plot
library(multiROC)
pred1_df <- as.data.frame(pred1_test)
names(pred1_df) <- c("E", "G", "N")

act1 <- test_set$gent_n_eligible

for_roc1 <- cbind(act1, pred1_df) %>%
  mutate(
    G1_true = case_when(act1 == "Eligible, Did Not Gentrify" ~ 1, TRUE ~ 0),
    G2_true = case_when(act1 == "Not Eligible for Gentification" ~ 1, TRUE ~ 0),
    G3_true = case_when(act1 == "Gentrified" ~ 1, TRUE ~ 0),
    G1_pred_m1 = E,
    G2_pred_m1 = N,
    G3_pred_m1 = G
  ) %>%
  select(G1_true, G2_true, G3_true, G1_pred_m1, G2_pred_m1, G3_pred_m1)


roc1 <- multi_roc(for_roc1)
for_plot_roc1 <- plot_roc_data(roc1)

for_plot_roc1 %>%
  filter(Group == "Macro") %>%
  ggplot(aes(y = Sensitivity, x = sort(Specificity))) +
  geom_line() +
  xlab("Specificity")



