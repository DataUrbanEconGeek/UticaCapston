library(rpostgis)
library(sp)
library(tigris)
source("helper00_project-db-connection.R")

rva_bound <- pgGetGeom(spatialdb, "rva_boundry")

#
va_tracts <- tracts("VA", cb = TRUE)

cb <- core_based_statistical_areas(cb = TRUE)

rva_cbs <- cb[grepl("Richmond, VA", cb$NAME),]

rva_cbs_tracts <- va_tracts[rva_cbs,]

rva_cbs_tracts <- spTransform(rva_cbs_tracts, CRS = proj4string(rva_bound))
rva_cbs <- spTransform(rva_cbs, CRS = proj4string(rva_bound))

# Save to DB
pgInsert(spatialdb, "rva_cbs_census_tracts", rva_cbs_tracts)
pgInsert(spatialdb, "rva_cbsa", rva_cbs)



