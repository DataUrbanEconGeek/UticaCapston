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
source("../helper_scripts/helper00_project-db-connection.R")

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
    age_in_years = 2019 - YrBuilt,
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
  mutate(
    stories_fixed = as.numeric(gsub("\\.{2}", ".", 
                                         gsub(",", ".", 
                                              trimws(str_remove(Stories, "\\+"),
                                                     "right")))),
    prime_const_type = str_to_title(trimws(gsub('[0-9]\"|[0-9]|[0-9]/[0-9]', "",
                                                ConstFr), "both"))
         ) %>%
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
rva_neighborhoods <- pgGetGeom(spatialdb, "rva_neighborhoods_gent")
rva_neighborhoods <- st_as_sf(rva_neighborhoods)

# Make into points sf.
buildings_sf <- st_as_sf(buildings_df, coords = c("lon", "lat"), 
                        crs = st_crs(rva_neighborhoods))

# Locate points from properties data that lie within polygons. 
buildings_w_nbh <- st_intersection(buildings_sf, rva_neighborhoods)
st_geometry(buildings_w_nbh) <- NULL

# Drop un-needed attributes, and rename neighborhood name attributes
buildings_w_nbh <- buildings_w_nbh %>%
  select(-SymbolID, -SHAPE_Leng, -SHAPE_Area) %>%
  rename(neighborhood = Name)
  
# Save property neighborhood assosiactions to DB
dbWriteTable(defaultdb, "properties_w_neighborhoods", buildings_w_nbh, 
             overwrite = TRUE, row.names = FALSE)

