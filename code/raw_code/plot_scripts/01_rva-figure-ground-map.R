###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: Make a map of the building footprints. 
##  
###############################################################################

library(rpostgis)
library(tmap)
source("helper00_project-db-connection.R")

# Load in Richmond building shape file.
rva_fp <- pgGetGeom(spatialdb, "rva_building_footprints")

# Load in Richmond's boundry.
rva_boundry <- pgGetGeom(spatialdb, "rva_boundry")

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
png_path <- "../../figures/exploratory_figures/03_city-richmond-figure-ground-map.png"
svg_path <- "../../figures/exploratory_figures/03_city-richmond-figure-ground-map.svg"

save_tmap(mapplot, png_path, width = 800, height = 800)
save_tmap(mapplot, svg_path, width = 800, height = 800)



