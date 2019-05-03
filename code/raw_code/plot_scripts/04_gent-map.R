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
    gent_n_egible_10 = case_when(
      gentrified_10 == "yes" ~ "Gentrified",
      gentrified_10 == "no" & eligibil_for_gentrification == "yes" ~ "Egible, Did Not Gentrify",
      TRUE ~ "Not Egible for Gentification"
    ),
    gent_n_egible_11 = case_when(
      gentrified_11 == "yes" ~ "Gentrified",
      gentrified_11 == "no" & eligibil_for_gentrification == "yes" ~ "Egible, Did Not Gentrify",
      TRUE ~ "Not Egible for Gentification"
    ),
    gent_n_egible_12 = case_when(
      gentrified_12 == "yes" ~ "Gentrified",
      gentrified_12 == "no" & eligibil_for_gentrification == "yes" ~ "Egible, Did Not Gentrify",
      TRUE ~ "Not Egible for Gentification"
    ),
    gent_n_egible_13 = case_when(
      gentrified_13 == "yes" ~ "Gentrified",
      gentrified_13 == "no" & eligibil_for_gentrification == "yes" ~ "Egible, Did Not Gentrify",
      TRUE ~ "Not Egible for Gentification"
    ),
    gent_n_egible_14 = case_when(
      gentrified_14 == "yes" ~ "Gentrified",
      gentrified_14 == "no" & eligibil_for_gentrification == "yes" ~ "Egible, Did Not Gentrify",
      TRUE ~ "Not Egible for Gentification"
    ),
    gent_n_egible_15 = case_when(
      gentrified_15 == "yes" ~ "Gentrified",
      gentrified_15 == "no" & eligibil_for_gentrification == "yes" ~ "Egible, Did Not Gentrify",
      TRUE ~ "Not Egible for Gentification"
    ),
    gent_n_egible_16 = case_when(
      gentrified_16 == "yes" ~ "Gentrified",
      gentrified_16 == "no" & eligibil_for_gentrification == "yes" ~ "Egible, Did Not Gentrify",
      TRUE ~ "Not Egible for Gentification"
    ),
    gent_n_egible_17 = case_when(
      gentrified_17 == "yes" ~ "Gentrified",
      gentrified_17 == "no" & eligibil_for_gentrification == "yes" ~ "Egible, Did Not Gentrify",
      TRUE ~ "Not Egible for Gentification"
    )
  )

gent_map_10 <- ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gent_n_egible_10, color = gent_n_egible_10)) +
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

gent_map_11 <- ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gent_n_egible_11, color = gent_n_egible_11)) +
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

gent_map_12 <- ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gent_n_egible_12, color = gent_n_egible_12)) +
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

gent_map_13 <- ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gent_n_egible_13, color = gent_n_egible_13)) +
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

gent_map_14 <- ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gent_n_egible_14, color = gent_n_egible_14)) +
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

gent_map_15 <- ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gent_n_egible_15, color = gent_n_egible_15)) +
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

gent_map_16 <- ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gent_n_egible_16, color = gent_n_egible_16)) +
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

gent_map_17 <- ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gent_n_egible_17, color = gent_n_egible_17)) +
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


