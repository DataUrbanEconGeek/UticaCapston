###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(RPostgreSQL)
library(getPass)
library(dplyr)

# Driver.
pgdrv <- dbDriver(drvName = "PostgreSQL")

# Connect to DB
db <-dbConnect(pgdrv, dbname="defaultdb",
               host="db-ubranecongeek-rva-51804-do-user-4688106-0.db.ondigitalocean.com", 
               port=25060, user = 'doadmin', 
               password = getPass("Enter Password:"))

# Load in Richmond Assessor's data from Data Warehouse
master_df <- dbGetQuery(db, "SELECT * from real_master")
land_df <- dbGetQuery(db, "SELECT * from real_land")
improve_df <- dbGetQuery(db, "SELECT * from real_improvement")

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


