###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(dplyr)
library(ggplot2)
library(ggmap)
source("helper00_project-db-connection.R")

# Load in Richmond Assessor's data from Data Warehouse
master_df <- dbGetQuery(defaultdb, "SELECT 'PIN', 
                              'LocAddr', 
                              'LocCity',
                              'LocState', 
                              'LocZip', 
                              'PCDesc', 
                              'AssocNam',
                              'NeiDesc'
                              from real_master")

land_df <- dbGetQuery(defaultdb, "SELECT 'PIN',
                            'WCovDes1',
                            'HeatDesc',
                            'RoofDesc',
                            'RoofMatD',
                            'CondDesc',
                            'ExtDesc2',
                            'ExtDesc1',
                            'ConstFr',
                            'Foundat',
                            'BldgType',
                            'Stories',
                            'YrBuilt',
                            'FinSize', 
                            'UseDesc'
                            from real_land")

improve_df <- dbGetQuery(defaultdb, "SELECT * from real_improvement")

# Select the first observations for each property by year built and BldgType
improv_sub_df <- improve_df %>%
  group_by(PIN) %>%
  arrange(YrBuilt, BldgType) %>%
  filter(YrBuilt == min(YrBuilt), row_number() == 1)

# Join master data and improvements data.
buildings_df <- master_df %>%
  inner_join(improv_sub_df, by = "PIN")


# Create a full address attribute to use for pulling coordinates. 
buildings_df$FullAddr <- paste(trimws(as.character(buildings_df$LocAddr),
                                      which = "right"), " Richmond", " VA", 
                               sep = ",")

# Use Google's Geocode API through the ggmaps package to pull longitudinal and
# latitudinal coordinates. Done in phases in order to avoid paying for data.
building_coord <- geocode(buildings_df$FullAddr[1:20000])
building_coord2 <- geocode(buildings_df$FullAddr[20001:22980])

# Bind coordinate data by rows
master_coords <- bind_rows(building_coord, building_coord2)

# Add in the property pins for coordinates.
master_coords$pin <- trimws(buildings_df$PIN[1:22980], which = "right")

# Write coordinates to data warehouse
dbWriteTable(defaultdb, "coordinate_lookup", master_coords, overwrite = TRUE)

# Keep a local back-up csv file.
write.csv(master_coords, file = "../../data/raw_data/coordinates.csv")

# Fix Pins in Building data frame
buildings_df$PIN <- trimws(buildings_df$PIN, which = "right")

# Start data frame that joins assessor data and coordinates.
add_coords <- buildings_df %>%
  inner_join(master_coords, by = c("PIN" = "pin"))

# Write full data-set to data warehouse
dbWriteTable(defaultdb, "master_buildings", add_coords, overwrite = TRUE)




