###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: Use to load Richmond Assessor's data from text  files into the Data
## Warehouse                                                                    
##                                                                           
##                                                                           
###############################################################################

library(RPostgreSQL)
library(getPass)

# Driver.
pgdrv <- dbDriver(drvName = "PostgreSQL")

# Connect to DB
db <-dbConnect(pgdrv, dbname="defaultdb",
               host="db-ubranecongeek-rva-51804-do-user-4688106-0.db.ondigitalocean.com", 
               port=25060, user = 'doadmin', 
               password = getPass("Enter Password:"))

# Load in Richmond Assessor's data.
master_df <- read.table("~/projects/rva/RealMaster.txt",
                        header = TRUE, sep = ",")
land_df <- read.table("~/projects/rva/RealLand.txt",
                      header = TRUE, sep = ",")
improve_df <- read.table("~/projects/rva/RealImprov.txt",
                         header = TRUE, sep = ",")

# Write to Data Warehouse
dbWriteTable(db, "real_master", master_df, overwrite = TRUE)
dbWriteTable(db, "real_land", land_df, overwrite = TRUE)
dbWriteTable(db, "real_improvement", improve_df, overwrite = TRUE)

