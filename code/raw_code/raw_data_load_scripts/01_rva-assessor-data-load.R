###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: Use to load Richmond Assessor's data from text files into the Data
## Warehouse                                                                    
##                                                                           
##                                                                           
###############################################################################

source("../helper_scripts/helper00_project-db-connection.R")

# Load in Richmond Assessor's data.
master_df <- read.table("../../../data/temp_data/RealMaster.txt",
                        header = TRUE, sep = ",")
land_df <- read.table("../../../data/temp_data/RealLand.txt",
                      header = TRUE, sep = ",")
improve_df <- read.table("../../../data/temp_data/RealImprov.txt",
                         header = TRUE, sep = ",")

# Write to Data Warehouse
dbWriteTable(defaultdb, "real_master", master_df, overwrite = TRUE)
dbWriteTable(defaultdb, "real_land", land_df, overwrite = TRUE)
dbWriteTable(defaultdb, "real_improvement", improve_df, overwrite = TRUE)

