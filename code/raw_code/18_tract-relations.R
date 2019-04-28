###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

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
  filter(state_code == "51" & (county_code == "760" |
                                 county_code == "007" |
                                 county_code == "025" |
                                 county_code == "033" |
                                 county_code == "036" |
                                 county_code == "041" |
                                 county_code == "049" |
                                 county_code == "053" |
                                 county_code == "057" |
                                 county_code == "065" |
                                 county_code == "075" |
                                 county_code == "081" |
                                 county_code == "085" |
                                 county_code == "087" |
                                 county_code == "095" |
                                 county_code == "097" |
                                 county_code == "099" |
                                 county_code == "101" |
                                 county_code == "109" |
                                 county_code == "127" |
                                 county_code == "135" |
                                 county_code == "145" |
                                 county_code == "147" |
                                 county_code == "149" |
                                 county_code == "175" |
                                 county_code == "177" |
                                 county_code == "179" |
                                 county_code == "181" |
                                 county_code == "183" |
                                 county_code == "570" |
                                 county_code == "670" |
                                 county_code == "730"))

dbWriteTable(defaultdb, "tract_relations_2000_2017", rva_tract_relations, 
             overwrite = TRUE, row.names = FALSE)


