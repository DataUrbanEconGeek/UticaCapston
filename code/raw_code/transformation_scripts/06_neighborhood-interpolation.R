###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
library(ggplot2)
library(sf)
library(units)
source("../helper_scripts/helper00_project-db-connection.R")
source("../helper_scripts/helper04_color_palette.R")

dbGetQuery(spatialdb, "SELECT * FROM pg_catalog.pg_tables WHERE 
           schemaname = 'public'") 


# Load tract geometries
rva_tracts <- pgGetGeom(spatialdb, "rva_census_tracts")
rva_tracts <- st_as_sf(rva_tracts)

# Load river geometry
james_river <- pgGetGeom(spatialdb, "rva_james_river")
james_river <- st_as_sf(james_river)


# load data on gentirfied tracts
gent_test_results <- dbGetQuery(defaultdb, 
                                "SELECT * from rva_gentrification_results")


# Join tract gentrification data
rva_tracts_n_gent <- rva_tracts %>%
  inner_join(gent_test_results, by = c("TRACTCE" = "tract")) %>%
  mutate(gent_n_egible = case_when(
    gentrified == "yes" ~ "Gentrified",
    gentrified == "no" & eligibil_for_gentrification == "yes" ~
      "Egible, Did Not Gentrify",
    TRUE ~ "Not Egible for Gentification"))

# Load neighborhood geometry
neighborhoods <- pgGetGeom(spatialdb, "rva_neighborhoods")
neighborhoods <- st_as_sf(neighborhoods)

# Add area in meters to neighborhoods
neighborhoods_2 <- neighborhoods %>%
  mutate(neighborhood_area = st_area(.) %>% as.numeric())

# intersect neighborhoods and tracts
tract_neighborhood <- st_intersection(neighborhoods_2, rva_tracts_n_gent)

# Calculate area of the intersections
tract_neighborhood <- tract_neighborhood %>%
  mutate(intersect_area = st_area(.) %>% as.numeric()) %>%
  mutate(pct_overlap = (intersect_area / neighborhood_area) * 100)

# find highest overlap of tracts  for each neighborhood
max_overlap <- tract_neighborhood %>%
  select(Name, pct_overlap) %>%
  group_by(Name) %>%
  summarise(max_overlap = max(pct_overlap))

st_geometry(max_overlap) <- NULL

assign_gent_status <- max_overlap %>%
  inner_join(y = (tract_neighborhood %>% 
                    select(Name, pct_overlap, gent_n_egible)), 
             by = c("Name" = "Name", "max_overlap" = "pct_overlap"))

assign_gent_status <- assign_gent_status %>%
  select(Name, gent_n_egible)

neighborhoods_3 <- neighborhoods %>%
  inner_join(assign_gent_status, by = "Name")

neighborhoods_3$gent_n_egible[31] <- "Gentrified"

gentmap_neighborhoods_master <- ggplot(neighborhoods_3) +
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

dest_path <- "../../../figures/exploratory_figures/05_gentried-map-neighborhoods.png"
ggsave(filename = dest_path, gentmap_neighborhoods_master)



