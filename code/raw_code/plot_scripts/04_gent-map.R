###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
library(ggplot2)
source("../helper_scripts/helper00_project-db-connection.R")
source("../helper_scripts/helper04_color_palette.R")

rva_gent <- dbGetQuery(defaultdb, "SELECT * from rva_gent_2000_2017")

#
rva_tracts <- pgGetGeom(spatialdb, "rva_census_tracts")
rva_tracts <- fortify(rva_tracts, region = "TRACTCE")


rva_tracts_n_gent <- rva_tracts %>%
  inner_join(rva_gent, by = c("id" = "tract")) %>%
  mutate(
    gent_n_egible = case_when(
      gentrified == "yes" ~ "Gentrified",
      gentrified == "no" & eligibil_for_gentrification == "yes" ~ "Egible, Did Not Gentrify",
      TRUE ~ "Not Egible for Gentification"
    )
  )

gent_map <- ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gent_n_egible, color = gent_n_egible)) +
  scale_color_manual(values = proj_palette[c(15, 11, 13)],
                     breaks = c("Gentrified", "Egible, Did Not Gentrify",
                                "Not Egible for Gentification")) +
  scale_fill_manual(values = proj_palette[c(7, 3, 9)], 
                    breaks = c("Gentrified", "Egible, Did Not Gentrify",
                               "Not Egible for Gentification")) +
  labs(fill = "Key", color = "Key") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

dest_path <- "../../../figures/exploratory_figures/04_gentried-map.png"
ggsave(filename = dest_path, gent_map)


