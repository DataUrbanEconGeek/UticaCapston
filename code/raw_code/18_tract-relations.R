library(dplyr)
source("helper00_project-db-connection.R")

url_tract_relationship <- "https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/va51trf.txt"

dest_file <- "../../data/raw_data/tract_2000_2010_relations"
download.file(url_tract_relationship, 
              destfile = dest_file)

tract_relations <- read.table(dest_file, sep = ",", colClasses = "character")

rva_tract_relations <- tract_relations %>%
  select(V1, V2, V4, V13) %>%
  rename(state_code = V1, county_code = V2, tract_2000 = V4, 
         tract_2017 = V13) %>%
  filter(state_code == "51" & county_code == "760")

dbWriteTable(defaultdb, "tract_relations_2000_2017", rva_tract_relations, 
             overwrite = TRUE, row.names = FALSE)


