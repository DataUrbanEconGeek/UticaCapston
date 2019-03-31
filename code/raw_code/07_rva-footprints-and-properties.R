###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(RPostgreSQL)
library(rpostgis)
library(sp)
library(getPass)
library(dplyr)
library(ggplot2)
library(ggmap)
library(tmap)
library(RColorBrewer)
library(wesanderson)

# Driver.
pgdrv <- dbDriver(drvName = "PostgreSQL")

# Connect to DBs.
defualt <-dbConnect(pgdrv, dbname="defaultdb",
               host="db-ubranecongeek-rva-51804-do-user-4688106-0.db.ondigitalocean.com", 
               port=25060, user = 'doadmin', 
               password = getPass("Enter Password:"))

spatial <-dbConnect(pgdrv, dbname="spatialdb",
                host="db-ubranecongeek-rva-51804-do-user-4688106-0.db.ondigitalocean.com", 
                port=25060, user = 'doadmin', 
                password = getPass("Enter Password:"))

# Load in master building data frame from Data Warehouse.
m_buildings_df <- dbGetQuery(defualt, "SELECT *
                             from master_buildings")

coor_na <- is.na(m_buildings_df$lon)
m_buildings_df2 <- subset(m_buildings_df, m_buildings_df$lon != 
                            is.na(m_buildings_df$lon))

# Load in footprints.
rva_fp <- pgGetGeom(spatial, "rva_building_footprints")

# Make into Spatial Points data frame.
property_spdf <- SpatialPointsDataFrame(m_buildings_df2[,25:26],
                                        m_buildings_df2[,1:24],
                                        proj4string = rva_fp@proj4string)

#
joined <- over(property_spdf, rva_fp)

#
m_buildings_df2 <- cbind(m_buildings_df2, joined)

m_buildings_df3 <- m_buildings_df2 %>%
  select(-one_of('PIN')) %>%
  group_by(FID) %>%
  filter(row_number() == 1)

m_buildings_df4 <- SpatialPointsDataFrame(m_buildings_df3[24:25],
                                          m_buildings_df3, 
                                          proj4string = rva_fp@proj4string)

#
rva_fp2 <- rva_fp
rva_fp2 <- merge(rva_fp, m_buildings_df3, by.x = "FID", by.y = "FID")

#
rva_fp3 <- rva_fp2[is.na(rva_fp2$YrBuilt) == FALSE, ]

rva_fp4 <- fortify(rva_fp3, region = "FID")

rva_fp5 <- merge(rva_fp4, rva_fp3@data, by.x = "id", by.y = "FID")
rva_fp6 <- fortify(rva_fp, region = "FID")


#

#
year_map <- ggplot(rva_fp6, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "green", color = "green",  alpha = 0.5, size = 0.1) +
  geom_polygon(data =  rva_fp5, aes(x = long, y = lat.x, group = group,
                                    fill = YrBuilt)) +
  scale_fill_gradient(low="red", high="blue") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

ggplotly(year_map)

filename <- "01_yr-built-map.png"
ggsave(filename = filename, year_map)




