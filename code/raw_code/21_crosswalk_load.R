###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
library(stringr)
source("helper00_project-db-connection.R")

dest_file <- "../../data/raw_data/crosswalk_2000_2010.csv"
crosswalk <- read.csv(dest_file, colClasses = "character")

county_codes <- c("760", "007", "025", "033", "036", "041", "049", "053", "057", 
                  "065", "075", "081", "085", "087", "095", "097", "099", "101", 
                  "109", "127", "135", "145", "147", "149", "175", "177", "179",
                  "181", "183", "570", "670", "730")

codes_part1 <- paste0("^51", county_codes, collapse = "|")

crosswalk_rva_csba <- crosswalk %>%
  filter(str_detect(trtid00, codes_part1))

dbWriteTable(defaultdb, "crosswalk_2000_2010", crosswalk_rva_csba, 
             overwrite = TRUE, row.names = FALSE)



