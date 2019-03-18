###############################################################################
## Author: Andrew Rosa                                                       ##
##                                                                           ##
## Notes: Code to combine Microsoft's US Building Footprint data with the US ##
## Census's geograpic Boundries data provided through the tigris package for ##
## Richmond Virginia.                                                        ##
##                                                                           ##
###############################################################################

library(rgdal)
library(rgeos)
library(tigris)
library(tmap)
library(sf)

shape <- readOGR(dsn = "/home/rstudio/projects/rva/r_rva_building_mapsVirginia.shp")


v_counties <- counties("Virginia")
rva <- v_counties[grep("Richmond", v_counties$NAME), ]

rva <- spTransform(rva, CRS = proj4string(shape))
rva <- rva[1,]

shape <- shape[as.vector(gIntersects(shape, rva, byid = TRUE)), ]


shape <- gSimplify(shape, tol = 0.00001)


mapplot <- tm_shape(rva)+
  tm_polygons(col = "white")+
  tm_shape(shape)+
  tm_polygons(col = "blue", border.alpha = 0)+
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

save_tmap(mapplot, "rva_building_footprints.png", width = 800, height = 800)