###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: This code loads the Virginia footprints shapefile. Uses the tigris API                                                                   
## to download Richmond's city boundry. Subsets the Virginia footprints to just 
## Richmond, then loads the footprints and city boundry into the data warehouse.
##                                                                           
###############################################################################

library(rpostgis)
library(tigris)
library(rgdal)
library(sp)
library(rgeos)
source("helper00_project-db-connection.R")

# Load in building footprints
shape <- readOGR(dsn = "../../data/temp_data/r_rva_building_mapsVirginia.shp")

# Query to download boundries of counties in Virginia.
v_counties <- counties("Virginia")

# Select Richmond's boundry.
rva <- v_counties[grep("Richmond", v_counties$NAME), ]

# Set Richomd's boundry spatial data frame to same type as the Richmonds 
# building spatial data frame.
rva <- spTransform(rva, CRS = proj4string(shape))
rva <- rva[1,]

# Create a table to hold RVA's boundry in the data warehouse, populate with the 
# spatial data.
pgInsert(spatialdb, "rva_boundry", rva)

# Intersect boundry and buildings. This will result in a subset of the buildings
# that are within the city boundries.
shape <- shape[as.vector(gIntersects(shape, rva, byid = TRUE)), ]

# Create RVA building footprints table in the data warehouse, populate with the 
# spatial data.
pgInsert(spatialdb, "rva_building_footprints", shape)
