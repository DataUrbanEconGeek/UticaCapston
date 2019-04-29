###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(dplyr)
source("../helper_scripts/helper00_project-db-connection.R")

# Load in Richmond Assessor's data from Data Warehouse
master_df <- dbGetQuery(defaultdb, "SELECT * from real_master")
land_df <- dbGetQuery(defaultdb, "SELECT * from real_land")
improve_df <- dbGetQuery(defaultdb, "SELECT * from real_improvement")

# Selesct distinct PINS from master and improvements data frames
dist_improve_pins <- improve_df %>%
  distinct(PIN)

dist_master_pins <- master_df %>%
  distinct(PIN)

# Find Pins from the master data frame that are not in the improvements data 
# frame  
missing_props <- dist_master_pins %>%
  anti_join(dist_improve_pins) %>%
  arrange(PIN)









