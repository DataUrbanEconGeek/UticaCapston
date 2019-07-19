###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
source("../helper_scripts/helper00_project-db-connection.R")


#CUSR0000SA0

cpis <- read.table(dest_file, header = TRUE, sep = "\t", 
                   stringsAsFactors = FALSE)

cpis_2000_2017 <- cpis %>%
  mutate(series_id = trimws(series_id, which = "right")) %>%
  filter(series_id == "CUSR0000SA0" & year >= 2000 & year <= 2017) %>%
  select(year, value) %>%
  group_by(year) %>%
  summarise(value = mean(value))

dbWriteTable(defaultdb, "cpi_2000_2017", cpis_2000_2017, overwrite = TRUE,
             row.names = FALSE)

