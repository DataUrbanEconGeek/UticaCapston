###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: Set up a postGIS data-base. 
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(RPostgreSQL)
library(rpostgis)
library(getPass)

# Driver.
pgdrv <- dbDriver(drvName = "PostgreSQL")

# Connect to DB.
db1 <-dbConnect(pgdrv, dbname="spatialdb",
               host="db-ubranecongeek-rva-51804-do-user-4688106-0.db.ondigitalocean.com", 
               port=25060, user = 'doadmin', 
               password = getPass("Enter Password:"))

# Extend data-base with postGIS.
dbSendQuery(db1, "CREATE EXTENSION IF NOT EXISTS postgis")
dbGetQuery(db1, "CREATE EXTENSION IF NOT EXISTS POSTGIS_topology")

# Check if data-base is set-up correctly. 
pgPostGIS(db1)
