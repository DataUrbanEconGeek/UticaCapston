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


# Load in footprints.
rva_neighborhoods <- pgGetGeom(spatialdb, "rva_neighborhoods_gent")
rva_neighborhoods <- st_as_sf(rva_neighborhoods)

# Load in footprints.
rva_footprints <- pgGetGeom(spatialdb, "rva_building_footprints")
rva_footprints <- st_as_sf(rva_footprints)

# Load in River
james_river <- pgGetGeom(spatialdb, "rva_james_river")
james_river <- st_as_sf(james_river)

# Load in Parks
rva_parks <- pgGetGeom(spatialdb, "rva_parks_open_spaces")
rva_parks <- st_as_sf(rva_parks)

# Adjust neighborhoods
rva_neighborhoods2 <- rva_neighborhoods %>%
  st_difference(james_river)

rva_neighborhoods3 <- rva_neighborhoods2

for(i in 1:length(rva_parks$Name)){
  park_subset <- rva_parks[i,]
  
  rva_neighborhoods3 <- rva_neighborhoods3 %>%
    st_difference(park_subset) %>%
    select(-Name.1, -SymbolID.2, -SHAPE_Leng.2, -SHAPE_Area.2)
  
  print((i / 176) * 100)
  
}


maymont <- rva_neighborhoods3 %>%
  filter(Name == "Maymont Park")

plot(maymont["Name"])

# Join
rva_joined <- st_join(rva_footprints, rva_neighborhoods3 %>%
                        mutate(neighborhood_area = st_area(.) %>% 
                                 as.numeric())) %>%
  mutate(fp_area = st_area(.) %>% as.numeric())

rva_joined_sp <- as_Spatial(rva_joined)
pgInsert(spatialdb, "rva_neighborhood_footprints", rva_joined_sp)

#
rva_areas <- rva_joined
st_geometry(rva_areas) <- NULL

rva_areas <- rva_areas %>%
  select(Name, gent_n_eligible, neighborhood_area, fp_area) %>%
  group_by(Name, gent_n_eligible, neighborhood_area) %>%
  summarise(total_fp_area = sum(fp_area)) %>%
  mutate(open_area = neighborhood_area - total_fp_area) %>%
  mutate(built_unbuilt_ratio = total_fp_area / open_area)
  

rva_areas %>%
  ggplot(aes(x = gent_n_eligible, y = built_unbuilt_ratio, 
             fill = gent_n_eligible)) +
  geom_boxplot()

# Save property neighborhood assosiactions to DB
dbWriteTable(defaultdb, "rva_neighborhood_density", rva_areas, overwrite = TRUE,
             row.names = FALSE)

carytown <- rva_joined %>%
  filter(Name == "Carytown")

plot(carytown["fp_area"])

city_center <- rva_joined %>%
  filter(Name == "City Center")

plot(city_center["fp_area"])


manchester <- rva_joined %>%
  filter(Name == "Manchester")

plot(manchester["fp_area"])

maymont2 <- rva_joined %>%
  filter(Name == "Maymont Park")

plot(maymont2["fp_area"])

stadium <- rva_joined %>%
  filter(Name == "Stadium")

plot(stadium["fp_area"])
  
monroe_ward <- rva_joined %>%
  filter(Name == "Monroe Ward")

plot(monroe_ward["fp_area"])

  


