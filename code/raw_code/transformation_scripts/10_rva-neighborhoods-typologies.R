###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(rpostgis)
library(RPostgreSQL)
library(sf)
library(dplyr)
library(ggplot2)
source("../helper_scripts/helper00_project-db-connection.R")

dbGetQuery(spatialdb, "SELECT * 
                        FROM pg_catalog.pg_tables 
                          WHERE schemaname = 'public'") 


# Load in neighborhoods.
rva_neighborhoods <- pgGetGeom(spatialdb, "rva_neighborhoods_gent")
rva_neighborhoods <- st_as_sf(rva_neighborhoods)

# Load in footprints.
rva_urban_types <- pgGetGeom(spatialdb, "rva_urban_types")
rva_urban_types <- st_as_sf(rva_urban_types)


# Add area in meters square to neighborhoods
neighborhoods_2 <- rva_neighborhoods %>%
  mutate(neighborhood_area = st_area(.) %>% as.numeric())

# intersect neighborhoods and tracts
neighborhood_types <- st_intersection(neighborhoods_2, rva_urban_types)

# Calculate area of the intersections
neighborhood_types <- neighborhood_types %>%
  mutate(intersect_area = st_area(.) %>% as.numeric()) %>%
  mutate(pct_overlap = (intersect_area / neighborhood_area) * 100)

#
non_geom <- rva_neighborhoods %>%
  select(Name, gent_n_eligible)
st_geometry(non_geom) <- NULL

neighborhood_types2 <- neighborhood_types
st_geometry(neighborhood_types2) <- NULL

types_pct <- neighborhood_types2 %>%
  select(Name, Typology, pct_overlap) %>%
  mutate(Typology = gsub("-", "_", gsub(" ", "_", Typology))) %>%
  spread(key = Typology, value = pct_overlap) %>%
  replace(., is.na(.), 0) %>%
  inner_join(non_geom, by = "Name")


#
types_pct %>%
  ggplot(aes(x = gent_n_eligible, y = Water, 
             fill = gent_n_eligible)) +
  geom_boxplot()

# Save property neighborhood assosiactions to DB
dbWriteTable(defaultdb, "rva_neighborhood_typology", types_pct, 
             overwrite = TRUE, row.names = FALSE)


  


