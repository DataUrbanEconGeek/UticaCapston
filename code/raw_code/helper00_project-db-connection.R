###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: Uses the RPostgeSQL package to connect to database hosted with 
## DigitalOcean. Sets PostgresSQLConnection objects to name of the schemas in 
## the database. 
##                                                                           
###############################################################################

library(RPostgreSQL)
library(getPass)

# Driver.
pgdrv <- dbDriver(drvName = "PostgreSQL")

# Host string.
hst <- "db-ubranecongeek-rva-51804-do-user-4688106-0.db.ondigitalocean.com"

# Schemas.
defaultdb <-dbConnect(pgdrv, dbname = "defaultdb",
               host = hst, 
               port = 25060, user = 'doadmin', 
               password = getPass("Enter Password:"))

spatialdb <-dbConnect(pgdrv, dbname = "spatialdb",
                host = hst, 
                port = 25060, user = 'doadmin', 
                passwor = getPass("Enter Password:"))

