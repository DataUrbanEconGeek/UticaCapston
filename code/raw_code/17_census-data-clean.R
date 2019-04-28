###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
source("helper00_project-db-connection.R")

cnames <- c("median_income", "median_home_value", "females_bach_deg", 
            "males_bach_deg", "total_pop")

census_2000_names <- c("new_income", "new_home_val", "new_female_bach", 
                       "new_male_bach", "new_pop")

acs_old_names <- c("B19013_001E", "B25077_001E", "B15002_032E", "B15002_015E", 
                   "B01003_001E")

census_2000 <- dbGetQuery(defaultdb, "SELECT * from corrected_2000_census")
acs_income_2017 <- dbGetQuery(defaultdb, "SELECT * from acs5_2017")

census_2000 <- census_2000 %>%
  mutate(state = str_sub(trtid10, 1, 2),
         county = str_sub(trtid10, 3, 5),
         tract = str_sub(trtid10, 6, 11)) %>%
  select(state, county, tract, new_income, new_home_val, new_female_bach,
         new_male_bach, new_pop)

cln_2000 <- census_2000 %>%
  rename_at(vars(census_2000_names), ~ cnames) %>%
  mutate(total_bach_deg = females_bach_deg + males_bach_deg) %>%
  mutate(percent_bach_deg = (total_bach_deg/total_pop)*100)

cln_2017 <- acs_income_2017 %>%
  rename_at(vars(acs_old_names), ~ cnames) %>%
  mutate(total_bach_deg = females_bach_deg + males_bach_deg) %>%
  mutate(percent_bach_deg = (total_bach_deg/total_pop)*100)

new_col_names <-c("state", "county", "tract", "mi_2000", "mhv_2000", "fbd_2000",
                  "mbd_2000", "tp_2000", "tbd_2000", "pbd_2000", "mi_2017", 
                  "mhv_2017", "fbd_2017", "mbd_2017", "tp_2017", "tbd_2017", 
                  "pbd_2017")

# Inflation 2000-2017 2.099%
cln_all <- cln_2000 %>%
  full_join(cln_2017, by = c("state", "county", "tract")) %>%
  rename_all(~new_col_names) %>%
  mutate(mi_2000_adj = mi_2000*1.02099^17,
         mhv_2000_adj = mhv_2000*1.02099^17) %>%
  mutate(in_chg = mi_2017 - mi_2000_adj,
         mhv_chg = mhv_2017 - mhv_2000_adj) %>%
  mutate(in_gl = 
           case_when(
             in_chg > 0 ~ "gain",
             in_chg < 0 ~ "loss"
           )
  )

dbWriteTable(defaultdb, "cln_census_2000_2017", cln_all, overwrite = TRUE,
             row.names = FALSE)

