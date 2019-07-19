###############################################################################
## Author: Andrew Rosa                                                       ##
##                                                                           ##
## Notes: Code to converts Microsoft's US Building Footprint data for        ##
## Virginia into a shp file from a geojson file.                             ##
##                                                                           ##
##                                                                           ##
###############################################################################
library(geojsonsf)
library(sf)

files <- list.files(path="../../data/temp_data",
                    pattern="*.geojson", full.names=TRUE, recursive=FALSE)

for(f in files){
  split <- read.table(text = f, sep = "/")
  filename <- split[,ncol(split)]
  state <- substr(filename,1,nchar(as.character(filename))-8)
  outfile <- paste0("../../data/temp_data",state,
                    ".shp")
  sf <- geojsonsf::geojson_sf(f)
  st_write(sf, outfile)
  print(paste0("Completed ",state))
}