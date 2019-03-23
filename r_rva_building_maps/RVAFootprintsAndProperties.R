###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(RPostgreSQL)
library(rpostgis)
library(getPass)
library(dplyr)
library(ggplot2)
library(ggmap)

# Driver.
pgdrv <- dbDriver(drvName = "PostgreSQL")

# Connect to DB.
db <-dbConnect(pgdrv, dbname="defaultdb",
               host="db-ubranecongeek-rva-51804-do-user-4688106-0.db.ondigitalocean.com", 
               port=25060, user = 'doadmin', 
               password = getPass("Enter Password:"))

# Load in master building data frame from Data Warehouse.
m_buildings_df <- dbGetQuery(db, "SELECT *
                             from master_buildings")




