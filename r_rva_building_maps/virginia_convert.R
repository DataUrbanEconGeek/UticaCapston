library(geojsonsf)
library(sf)

files <- list.files(path="/home/rstudio/projects/rva",
                    pattern="*.geojson", full.names=TRUE, recursive=FALSE)

for(f in files){
  split <- read.table(text = f, sep = "/")
  filename <- split[,ncol(split)]
  state <- substr(filename,1,nchar(as.character(filename))-8)
  outfile <- paste0("/home/rstudio/projects/rva/r_rva_building_maps",state,
                    ".shp")
  sf <- geojsonsf::geojson_sf(f)
  st_write(sf, outfile)
  print(paste0("Completed ",state))
}