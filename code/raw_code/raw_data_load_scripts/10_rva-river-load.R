###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(rpostgis)
library(tigris)
library(rgdal)
library(sp)
library(rgeos)
source("../helper_scripts/helper00_project-db-connection.R")


dbGetQuery(spatialdb, "SELECT * FROM pg_catalog.pg_tables WHERE 
           schemaname = 'public'") 

# Load in river shape
shape <- readOGR(dsn = "../../../data/temp_data/for_capstone/River/River.shp")

# Bring in an example to get proj4string
rva_bound <- pgGetGeom(spatialdb, "rva_boundry")

# Set Richomd's boundry spatial data frame to same type as the Richmonds 
# building spatial data frame.
james_river <- spTransform(shape, CRS = proj4string(rva_bound))

# Save to DB
pgInsert(spatialdb, "rva_james_river", james_river)







