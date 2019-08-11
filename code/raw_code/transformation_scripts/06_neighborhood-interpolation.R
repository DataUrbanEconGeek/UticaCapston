###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
library(ggplot2)
library(sf)
library(rpostgis)
library(units)
source("../helper_scripts/helper00_project-db-connection.R")

dbGetQuery(spatialdb, "SELECT * 
                        FROM pg_catalog.pg_tables 
                          WHERE schemaname = 'public'") 


# Load tract geometries
rva_tracts <- pgGetGeom(spatialdb, "rva_census_tracts")
rva_tracts <- st_as_sf(rva_tracts)

# load data on gentirfied tracts
gent_test_results <- dbGetQuery(defaultdb, 
                                "SELECT * from rva_gentrification_results")

# Join tract gentrification data
rva_tracts_n_gent <- rva_tracts %>%
  inner_join(gent_test_results, by = c("TRACTCE" = "tract")) %>%
  mutate(gent_n_eligible = case_when(
    gentrified == "yes" ~ "Gentrified",
    gentrified == "no" & eligibil_for_gentrification == "yes" ~
      "Eligible, Did Not Gentrify",
    TRUE ~ "Not Eligible for Gentification"))

# Load neighborhood geometry
neighborhoods <- pgGetGeom(spatialdb, "rva_neighborhoods")
neighborhoods <- st_as_sf(neighborhoods)

# Add area in meters square to neighborhoods
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

# assign a gentrification status for each neighborhood
assign_gent_status <- max_overlap %>%
  inner_join(y = (tract_neighborhood %>% 
                    select(Name, pct_overlap, gent_n_eligible)), 
             by = c("Name" = "Name", "max_overlap" = "pct_overlap")) %>%
  select(Name, gent_n_eligible)

neighborhoods_3 <- neighborhoods %>%
  inner_join(assign_gent_status, by = "Name")

neighborhoods_3$gent_n_eligible[31] <- "Gentrified"

# Load neighborhood boundries with gentrification data to DB 
neighborhoods_sp <- as_Spatial(neighborhoods_3)
pgInsert(spatialdb, "rva_neighborhoods_gent", neighborhoods_sp, 
         overwrite = TRUE)

