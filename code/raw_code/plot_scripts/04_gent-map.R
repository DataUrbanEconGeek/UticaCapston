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

# functions
tract_merger <- function(x, year){
  rva_tracts_n_gent1 <- rva_tracts %>%
    inner_join(x, by = c("id" = "tract"))
  test_var1_name <- "gentrified_t3"
  test_var1 <- rva_tracts_n_gent1[, grep(test_var1_name, 
                                         names(rva_tracts_n_gent1))]
  test_var2 <- rva_tracts_n_gent1[, grep("eligibil_for_gentrification", 
                                         names(rva_tracts_n_gent1))]
  expre <- list(
    quo(test_var1 == "yes" ~ "Gentrified"),
    quo(test_var1 == "no" & test_var2 == "yes" ~ "Egible, Did Not Gentrify"),
    quo(TRUE ~ "Not Egible for Gentification")
  )
  rva_tracts_n_gent2 <- rva_tracts_n_gent1 %>%
    mutate(gent_n_egible = case_when(!!!expre))
  return(rva_tracts_n_gent2)
}

gent_mapper <- function(x, year){
  title_name <- paste0("Richmond 2000-", i)
  gentmap <- ggplot(x, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = gent_n_egible, color = gent_n_egible)) +
    scale_color_manual(values = proj_palette[c(15, 11, 13)],
                       breaks = c("Gentrified", "Egible, Did Not Gentrify",
                                  "Not Egible for Gentification")) +
    scale_fill_manual(values = proj_palette[c(7, 3, 9)], 
                      breaks = c("Gentrified", "Egible, Did Not Gentrify",
                                 "Not Egible for Gentification")) +
    labs(fill = "Key", color = "Key", title = title_name) +
    theme_minimal() +
    theme(axis.text = element_blank(), panel.grid = element_blank(),
          axis.title = element_blank())
  return(gentmap)
}

# Load tract geometries
rva_tracts <- pgGetGeom(spatialdb, "rva_census_tracts")
rva_tracts <- fortify(rva_tracts, region = "TRACTCE")

# load data on gentirfied tracts
for(i in 2010:2017){
  query_string <- paste0("SELECT * from rva_gent_2000_", i)
  df_name <- paste0("rva_gent_", i, "_data")
  temp_df <- dbGetQuery(defaultdb, query_string)
  assign(df_name, temp_df)
}

gent_test_results <- dbGetQuery(defaultdb, 
                                "SELECT * from rva_gentrification_results")

# Merge gentrification data with tract geometries
for(i in 2010:2017){
  retrieve <- paste0("rva_gent_", i, "_data")
  df_name <- paste0("rva_tracts_n_gent_", i)
  temp_df1 <- get(retrieve)
  temp_df2 <- tract_merger(temp_df1, i)
  assign(df_name, temp_df2)
}

# Construct Map
for(i in 2010:2017){
  retrieve <- paste0("rva_tracts_n_gent_", i)
  df_name <- paste0("gent_map_", i)
  gent_map <- gent_mapper(get(retrieve), i)
  assign(df_name, gent_map)
}

# Save maps
for(i in 2010:2017){
  retrieve <- paste0("gent_map_", i)
  dest_path <- paste0("../../../figures/exploratory_figures/04_gentried-map", 
                      i, ".png")
  ggsave(filename = dest_path, get(retrieve))
}

#
rva_tracts_n_gent <- rva_tracts %>%
  inner_join(gent_test_results, by = c("id" = "tract")) %>%
  mutate(gent_n_egible = case_when(
    gentrified == "yes" ~ "Gentrified",
    gentrified == "no" & eligibil_for_gentrification == "yes" ~
      "Egible, Did Not Gentrify",
    TRUE ~ "Not Egible for Gentification"))


gentmap_master <- ggplot(rva_tracts_n_gent, 
                         aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gent_n_egible, color = gent_n_egible)) +
  scale_color_manual(values = proj_palette[c(15, 11, 13)],
                     breaks = c("Gentrified", "Egible, Did Not Gentrify",
                                "Not Egible for Gentification")) +
  scale_fill_manual(values = proj_palette[c(7, 3, 9)], 
                    breaks = c("Gentrified", "Egible, Did Not Gentrify",
                               "Not Egible for Gentification")) +
  labs(fill = "Key", color = "Key", 
       title = "Gentrification in Richmond \n2000-2017") +
  theme_minimal() +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        axis.title = element_blank())


