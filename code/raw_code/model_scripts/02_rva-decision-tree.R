###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(rpostgis)
library(RPostgreSQL)
library(sf)
library(dplyr)
library(stringr)
source("../helper_scripts/helper00_project-db-connection.R")

dbGetQuery(spatialdb, "SELECT * 
                        FROM pg_catalog.pg_tables 
                          WHERE schemaname = 'public'") 


# Load
prop_nbh <- dbGetQuery(defaultdb, "SELECT * from properties_w_neighborhoods")

nbh_density <- dbGetQuery(defaultdb, "SELECT * from rva_neighborhood_density")

#
  
# Save property neighborhood assosiactions to DB
dbWriteTable(defaultdb, "properties_w_neighborhoods", buildings_w_nbh, 
             overwrite = TRUE, row.names = FALSE)

