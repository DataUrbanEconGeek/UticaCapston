###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(rpostgis)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
source("helper_scripts/helper00_project-db-connection.R")

# Load in master building data frame from Data Warehouse.
m_buildings_df <- dbGetQuery(defaultdb, "SELECT * from master_buildings")

buildings_df <- m_buildings_df %>%
  filter(lon != is.na(m_buildings_df$lon)) %>%
  select(PIN, LocAddr, LocZip, PCDesc, AssocNam, NeiDesc, ImprType, UseDesc, 
         BldgType, Stories, YrBuilt, FinSize, ConstFr, CondDesc, RoofDesc,
         NumRms, NumBdRms, Num2Baths, Num3Baths, LastUpdD, lon, lat)

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

# Subset where spatial data frame where year is not missing.
with_yrbuilt <- buildings_w_fp %>%
  filter(YrBuilt != is.na(buildings_w_fp$YrBuilt))

# 
ids <- with_yrbuilt$FID
un_mantched <- rva_fp %>%
  filter(!FID %in% ids)


#
year_map <- ggplot(un_mantched) +
  geom_sf(fill = "gray", color = "gray",  alpha = 0.5, size = 0.1) +
  geom_sf(data =  with_yrbuilt, aes(color = YrBuilt, fill = YrBuilt), 
          size = 0.1) +
  coord_sf(datum = NA) +
  scale_fill_gradient(low = "red", high = "blue") +
  scale_color_gradient(low = "red", high = "blue") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())


filename <- "../../figures/exploratory_figures/01_yr-built-map-4.png"
ggsave(filename = filename, year_map)


rva_fp7 <- rva_fp5 %>%
  mutate(
    time_range = case_when(
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
  mutate(Stories_fixed = as.numeric(gsub("\\.{2}", ".", gsub(",", ".", 
                              trimws(str_remove(Stories, "\\+"), "right")
                              )))) %>%
  mutate(
    stories_new = case_when(
      Stories_fixed < 2 ~ "Less than 2 stories",
      Stories_fixed >= 2 & Stories_fixed < 3 ~ "2-3 stories",
      Stories_fixed >= 3 & Stories_fixed < 4 ~ "3-4 stories",
      Stories_fixed >= 4 & Stories_fixed < 5 ~ "4-5 stories",
      Stories_fixed >= 5 ~ "5 or more stories",
      TRUE ~ "NA"
    )
         )

year_map2 <- ggplot(rva_fp_ex, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray", color = "gray",  alpha = 0.5, size = 0.1) +
  geom_polygon(data =  rva_fp7, aes(x = long, y = lat.x, group = group,
                                    fill = time_range)) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

filename <- "../../../figures/exploratory_figures/01_yr-built-map-2.png"
ggsave(filename = filename, year_map2)

stories_map <- ggplot(rva_fp_ex, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray", color = "gray",  alpha = 0.2, size = 0.1) +
  geom_polygon(data =  rva_fp7, aes(x = long, y = lat.x, group = group,
                                    fill = stories_new)) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

filename <- "../../../figures/exploratory_figures/01_stories-map-2.png"
ggsave(filename = filename, stories_map)

rva_samp <- sample_n(rva_fp7, size = 1000)

yrbl_stories <- rva_samp %>%
  ggplot(aes(x = YrBuilt, y = Stories_fixed)) +
  geom_point() +
  theme_minimal()

rva_subset <- rva_samp %>%
  select(YrBuilt, Stories_fixed, lon, lat.x)

cluster <- kmeans(rva_subset, centers = 2)

ggplotly(year_map2)
