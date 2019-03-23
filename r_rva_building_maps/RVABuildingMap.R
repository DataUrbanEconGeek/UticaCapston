###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: Make a map of the building footprints. 
##  
###############################################################################

library(RPostgreSQL)
library(rpostgis)
library(rgdal)
library(rgeos)
library(tigris)
library(tmap)
library(sf)

# Driver.
pgdrv <- dbDriver(drvName = "PostgreSQL")

# Connect to DB.
db1 <-dbConnect(pgdrv, dbname="spatialdb",
                host="db-ubranecongeek-rva-51804-do-user-4688106-0.db.ondigitalocean.com", 
                port=25060, user = 'doadmin', 
                password = getPass("Enter Password:"))


# Load in Richmond building shape file.
rva_fp <- pgGetGeom(db1, "rva_building_footprints")

# Load in Richmond's boundry.
rva_boundry <- pgGetGeom(db1, "rva_boundry")

# Plot out map.
mapplot <- tm_shape(rva_boundry) +
  tm_polygons(col = "white") +
  tm_shape(rva_fp)+
  tm_polygons(col = "blue", border.alpha = 0) +
  tm_layout(
    fontfamily = "Baskerville",
    title.color = "blue",
    title.bg.color = "white",
    title = paste0(toupper("Richmond")),
    title.position = c("right", "bottom"),
    title.size = 0.25 * 800 / 1000,
    frame = TRUE,
    outer.margin = 0.01,
    inner.margin = c(0.028, 0.01, 0.01, 0.01)
  )

# Save map.
save_tmap(mapplot, "rva_building_footprints.png", width = 800, height = 800)