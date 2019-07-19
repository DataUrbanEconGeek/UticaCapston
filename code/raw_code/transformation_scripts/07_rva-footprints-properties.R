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
library(stringr)
source("helper_scripts/helper00_project-db-connection.R")

dbGetQuery(spatialdb, "SELECT * 
                        FROM pg_catalog.pg_tables 
                          WHERE schemaname = 'public'") 


# Load in master building data frame from Data Warehouse.
m_buildings_df <- dbGetQuery(defaultdb, "SELECT * from master_buildings")

# Select attributes that will be used for analysis later and create new
# attributes from existing attributes.
buildings_df <- m_buildings_df %>%
  filter(lon != is.na(m_buildings_df$lon)) %>%
  select(PIN, LocAddr, LocZip, PCDesc, AssocNam, NeiDesc, ImprType, UseDesc, 
         BldgType, Stories, YrBuilt, FinSize, ConstFr, CondDesc, RoofDesc,
         NumRms, NumBdRms, Num2Baths, Num3Baths, LastUpdD, lon, lat) %>%
  mutate(
    buit_time_range = case_when(
      YrBuilt < 1840 ~ "Before 1840",
      YrBuilt >= 1840 & YrBuilt < 1880 ~ "1840-1879",
      YrBuilt >= 1880 & YrBuilt < 1900 ~ "1880-1899",
      YrBuilt >= 1900 & YrBuilt < 1920 ~ "1900-1919",
      YrBuilt >= 1920 & YrBuilt < 1940 ~ "1920-1939",
      YrBuilt >= 1940 & YrBuilt < 1960 ~ "1940-1959",
      YrBuilt >= 1960 & YrBuilt < 1980 ~ "1960-1979",
      YrBuilt >= 1980 & YrBuilt < 2000 ~ "1980-1999",
      YrBuilt >= 2000 & YrBuilt < 2020 ~ "2000-2019",
      TRUE ~ "Unknown"
    )
  ) %>%
  mutate(stories_fixed = as.numeric(gsub("\\.{2}", ".", 
                                         gsub(",", ".", 
                                              trimws(str_remove(Stories, "\\+"),
                                                     "right")
  )))) %>%
  mutate(
    stories_new = case_when(
      stories_fixed < 2 ~ "Less than 2 stories",
      stories_fixed >= 2 & stories_fixed < 3 ~ "2-3 stories",
      stories_fixed >= 3 & stories_fixed < 4 ~ "3-4 stories",
      stories_fixed >= 4 & stories_fixed < 5 ~ "4-5 stories",
      stories_fixed >= 5 ~ "5 or more stories",
      TRUE ~ "NA"
    )
  )

# Load in footprints.
rva_fp <- pgGetGeom(spatialdb, "rva_building_footprints")
rva_fp <- st_as_sf(rva_fp)

# Make into points sf.
buildings_sf <- st_as_sf(buildings_df, coords = c("lon", "lat"), 
                        crs = st_crs(rva_fp))

# Locate points from properties data that lie within polygons. 
buildings_w_fids <- st_intersection(buildings_sf, rva_fp)
st_geometry(buildings_w_fids) <- NULL

# Join property data to building footprints
buildings_w_fp <- rva_fp %>%
  inner_join(buildings_w_fids, by = "FID")

# Save footprints with property data to DB
buildings_w_fp_sp <- as_Spatial(buildings_w_fp)
pgInsert(spatialdb, "rva_building_footprints_w_properties", buildings_w_fp_sp)

# Find polygons that didn't have associated property data
fids <- buildings_w_fp$FID
un_mantched_fps <- rva_fp %>%
  filter(!FID %in% fids)

# Save footprints with unmatched properties to DB
un_mantched_fps_sp <- as_Spatial(un_mantched_fps)
pgInsert(spatialdb, "rva_building_footprints_unmatched", un_mantched_fps_sp)

# Find properties that didn't have an associated polygon
pins <- buildings_w_fp$PIN
un_mantched_properties <- buildings_df %>%
  filter(!PIN %in% pins)

dbWriteTable(defaultdb, "unmatched_properties", un_mantched_properties, 
             overwrite = TRUE, row.names = FALSE)



