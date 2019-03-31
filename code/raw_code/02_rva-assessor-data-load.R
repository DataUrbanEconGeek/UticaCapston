###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: Use to load Richmond Assessor's data from text files into the Data
## Warehouse                                                                    
##                                                                           
##                                                                           
###############################################################################

source("helper00_project-db-connection.R")

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

