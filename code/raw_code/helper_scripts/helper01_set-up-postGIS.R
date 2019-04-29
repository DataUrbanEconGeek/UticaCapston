###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: Set up a postGIS data-base. 
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(rpostgis)
source("helper00_project-db-connection.R")

# Extend data-base with postGIS.
dbSendQuery(spatialdb, "CREATE EXTENSION IF NOT EXISTS postgis")
dbGetQuery(spatialdb, "CREATE EXTENSION IF NOT EXISTS POSTGIS_topology")

# Check if data-base is set-up correctly. 
pgPostGIS(spatialdb)
