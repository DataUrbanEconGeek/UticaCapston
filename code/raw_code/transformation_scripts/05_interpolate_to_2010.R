###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
library(stringr)
source("../helper_scripts/helper00_project-db-connection.R")

file_path <- "../../data/raw_data/crosswalk_2000_2010.csv"
crosswalk <- read.csv(file_path, colClasses = "character")

sf3_2000 <- dbGetQuery(defaultdb, "SELECT * from sf3_2000")

sf3_2000$trtid00 <- paste0(sf3_2000$state, sf3_2000$county, sf3_2000$tract)

sf3_2000 <- sf3_2000[, 4:11]

county_codes <- c("760", "007", "025", "033", "036", "041", "049", "053", "057", 
                  "065", "075", "081", "085", "087", "095", "097", "099", "101", 
                  "109", "127", "135", "145", "147", "149", "175", "177", "179",
                  "181", "183", "570", "670", "730")

codes_part1 <- paste0("^51", county_codes, collapse = "|")

crosswalk_rva_csba <- crosswalk %>%
  filter(str_detect(trtid00, codes_part1))

merge_with_crosswalk <- sf3_2000 %>%
  full_join(crosswalk_rva_csba, by = "trtid00") %>%
  mutate(weight = as.numeric(weight)) %>%
  mutate(m_in_w = P053001 * P052001,
         m_h_w = H076001 * H001001) %>%
  mutate(new_income = m_in_w * weight,
         new_home_val = m_h_w * weight,
         new_female_bach = P037032 * weight,
         new_male_bach = P037015 * weight,
         new_pop = P001001 * weight) %>%
  select(trtid10, new_income, P052001, new_home_val, H001001, new_female_bach, 
         new_male_bach, new_pop) %>%
  group_by(trtid10) %>%
  summarise_all(sum) %>%
  mutate(new_income = new_income / P052001, 
         new_home_val = new_home_val / H001001) %>%
  select(trtid10, new_income, new_home_val, new_female_bach, 
         new_male_bach, new_pop)
  
dbWriteTable(defaultdb, "corrected_2000_census", merge_with_crosswalk, 
             overwrite = TRUE, row.names = FALSE)


