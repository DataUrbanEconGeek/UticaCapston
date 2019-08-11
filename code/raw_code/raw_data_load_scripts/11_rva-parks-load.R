###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(rpostgis)
library(rgdal)
library(sp)
library(rgeos)
source("../helper_scripts/helper00_project-db-connection.R")


dbGetQuery(spatialdb, "SELECT * FROM pg_catalog.pg_tables WHERE 
           schemaname = 'public'") 

# Load in parks and open spaces shapes
shape <- readOGR(dsn = "../../../data/temp_data/for_capstone2/open_spaces/parks_open_spacs.shp")

# Bring in an example to get proj4string
rva_bound <- pgGetGeom(spatialdb, "rva_boundry")

# Match CRS
parks <- spTransform(shape, CRS = proj4string(rva_bound))

# Save to DB
pgInsert(spatialdb, "rva_parks_open_spaces", parks)







