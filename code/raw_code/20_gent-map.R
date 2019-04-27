library(dplyr)
library(ggplot2)
source("helper00_project-db-connection.R")

rva_gent <- dbGetQuery(defaultdb, "SELECT * from rva_gent_2000_2017")

#
rva_tracts <- pgGetGeom(spatialdb, "rva_census_tracts")
rva_tracts <- fortify(rva_tracts, region = "TRACTCE")


rva_tracts_n_gent <- rva_tracts %>%
  inner_join(rva_gent, by = c("id" = "tract"))

gent_map <- ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gentrified, color = gentrified), alpha = 0.5) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

dest_path <- "../../figures/exploratory_figures/gentried_map.png"
ggsave(filename = dest_path, gent_map)


