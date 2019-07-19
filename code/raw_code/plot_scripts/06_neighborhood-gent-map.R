###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(ggplot2)
library(sf)
library(rpostgis)
source("../helper_scripts/helper00_project-db-connection.R")
source("../helper_scripts/helper04_color-palette.R")

dbGetQuery(spatialdb, "SELECT * 
                        FROM pg_catalog.pg_tables 
                          WHERE schemaname = 'public'") 


# Load neighborhood with gentrification classes geometries
neighborhood_gent <- pgGetGeom(spatialdb, "rva_neighborhoods_gent")
neighborhood_gent <- st_as_sf(neighborhood_gent)

# Load river geometry
james_river <- pgGetGeom(spatialdb, "rva_james_river")
james_river <- st_as_sf(james_river)

# Build Map
gentmap_neighborhoods_master <- ggplot(neighborhood_gent) +
  geom_sf(aes(fill = gent_n_egible, color = gent_n_egible)) +
  geom_sf(data = james_river) +
  coord_sf(datum = NA) +
  scale_color_manual(values = proj_palette[c(15, 11, 13)],
                     breaks = c("Gentrified", "Egible, Did Not Gentrify",
                                "Not Egible for Gentification")) +
  scale_fill_manual(values = proj_palette[c(7, 3, 9)], 
                    breaks = c("Gentrified", "Egible, Did Not Gentrify",
                               "Not Egible for Gentification")) +
  labs(fill = "Key", color = "Key", 
       title = "Gentrification in Richmond \n2000-2017") +
  theme_minimal() +
  theme(axis.text = element_blank(), panel.grid = element_blank())

# Save Map
dest_path <- "../../../figures/exploratory_figures/05_gentried-map-neighborhoods.svg"
ggsave(filename = dest_path, gentmap_neighborhoods_master)



