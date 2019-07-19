###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(rpostgis)
library(sp)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(tmap)
library(RColorBrewer)
library(wesanderson)
source("helper_scripts/helper00_project-db-connection.R")

# Load in master building data frame from Data Warehouse.
m_buildings_df <- dbGetQuery(defaultdb, "SELECT * from master_buildings")

m_buildings_df <- subset(m_buildings_df, m_buildings_df$lon != 
                            is.na(m_buildings_df$lon))

# Load in footprints.
rva_fp <- pgGetGeom(spatialdb, "rva_building_footprints")

# Make into Spatial Points data frame.
property_spdf <- SpatialPointsDataFrame(m_buildings_df[,91:92],
                                        m_buildings_df[,1:24],
                                        proj4string = rva_fp@proj4string)

# Locate points from properties data that lie within polygons. This will provide
# a data frame with the FID if the property is in a polygon, and NA if not.
joined <- over(property_spdf, rva_fp)

# Add the FID's into properties data
m_buildings_df <- cbind(m_buildings_df, joined)

# Multiple FIDs mapped to single properties, making duplicates, select first 
# observation.
m_buildings_df2 <- m_buildings_df %>%
  select(-one_of('PIN')) %>%
  group_by(FID) %>%
  filter(row_number() == 1)


# Merge property data with spatial data by FID
rva_fp2 <- sp::merge(rva_fp, m_buildings_df2, by.x = "FID", by.y = "FID")

# Subset where spatial data frame where year is not missing.
rva_fp3 <- rva_fp2[is.na(rva_fp2$YrBuilt) == FALSE, ]

# Convert back into a regular data frame for use with ggplot2
rva_fp4 <- fortify(rva_fp3, region = "FID")

# 
ids <- unique(rva_fp4$id)
not_matched_props <- subset(m_buildings_df, m_buildings_df$FID != ids)

# The fortified data frame only contains needed spatial data, merge the dropped
# variables back in.
rva_fp5 <- merge(rva_fp4, rva_fp3@data, by.x = "id", by.y = "FID")

# Fortify original spatial dataframe for use in ggplot2
rva_fp6 <- fortify(rva_fp, region = "FID")

# Anti join to exclude buidling that have propertie data
rva_fp_ex <- rva_fp6 %>%
  anti_join(rva_fp5, by = "id")

#
year_map <- ggplot(rva_fp_ex, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray", color = "gray",  alpha = 0.5, size = 0.1) +
  geom_polygon(data =  rva_fp5, aes(x = long, y = lat.x, group = group,
                                    fill = YrBuilt)) +
  scale_fill_gradient(low="red", high="blue") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

ggplotly(year_map)

filename <- "../../../figures/exploratory_figures/01_yr-built-map.png"
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
