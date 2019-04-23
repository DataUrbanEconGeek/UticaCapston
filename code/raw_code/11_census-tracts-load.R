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
source("helper00_project-db-connection.R")


dbGetQuery(spatialdb, "SELECT * FROM pg_catalog.pg_tables WHERE 
           schemaname = 'public'") 

# Download Richmond Census Tracts. Richmopnd FIPS = 760
rva_tracts <- tracts(state = "Virginia", county = "760")


# Bring in an example to get proj4string
rva_bound <- pgGetGeom(spatialdb, "rva_boundry")

# Set Richomd's boundry spatial data frame to same type as the Richmonds 
# building spatial data frame.
rva_tracts <- spTransform(rva_tracts, CRS = proj4string(rva_bound))

# Save to DB
pgInsert(spatialdb, "rva_census_tracts", rva_tracts)







