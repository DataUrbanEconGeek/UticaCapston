###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
source("../helper_scripts/helper00_project-db-connection.R")


acs_cln_enh <- function(df, year){
  acs_old_names <- c("B19013_001E", "B25077_001E", "B15002_032E", "B15002_015E", 
                     "B01003_001E")
  new_names_start <- c("median_income", "median_home_value", "females_bach_deg", 
                       "males_bach_deg", "total_pop")
  acs_new_names <- paste0(new_names_start, "_", year)
  tot_bach_deg_exp <- paste0(acs_new_names[3], " + ", acs_new_names[4])
  perc_bach_deg_exp <- paste0("(total_bach_deg / ", acs_new_names[3], ") * 100")
  df_cln <- df %>%
    rename_at(vars(acs_old_names), ~ acs_new_names) %>%
    mutate_(total_bach_deg = tot_bach_deg_exp) %>%
    mutate_(percent_bach_deg = perc_bach_deg_exp)
  colnames(df_cln)[9] <- paste0("total_bach_deg", "_", year)
  colnames(df_cln)[10] <- paste0("percent_bach_deg", "_", year)
  return(df_cln)
} 




# Load data
census_2000 <- dbGetQuery(defaultdb, "SELECT * from corrected_2000_census")
acs_2010_data <- dbGetQuery(defaultdb, "SELECT * from acs5_2010")
acs_2011_data <- dbGetQuery(defaultdb, "SELECT * from acs5_2011")
acs_2012_data <- dbGetQuery(defaultdb, "SELECT * from acs5_2012")
acs_2013_data <- dbGetQuery(defaultdb, "SELECT * from acs5_2013")
acs_2014_data <- dbGetQuery(defaultdb, "SELECT * from acs5_2014")
acs_2015_data <- dbGetQuery(defaultdb, "SELECT * from acs5_2015")
acs_2016_data <- dbGetQuery(defaultdb, "SELECT * from acs5_2016")
acs_2017_data <- dbGetQuery(defaultdb, "SELECT * from acs5_2017")

# 200 sf3 census clean
census_2000_names <- c("new_income", "new_home_val", "new_female_bach", 
                       "new_male_bach", "new_pop")

census_2000 <- census_2000 %>%
  mutate(state = str_sub(trtid10, 1, 2),
         county = str_sub(trtid10, 3, 5),
         tract = str_sub(trtid10, 6, 11)) %>%
  select(state, county, tract, new_income, new_home_val, new_female_bach,
         new_male_bach, new_pop)

cnames <- c("median_income_2000", "median_home_value_2000", 
            "females_bach_deg_2000", "males_bach_deg_2000", "total_pop_2000")

cln_2000 <- census_2000 %>%
  rename_at(vars(census_2000_names), ~ cnames) %>%
  mutate(total_bach_deg_2000 = females_bach_deg_2000 + males_bach_deg_2000) %>%
  mutate(percent_bach_deg_2000 = (total_bach_deg_2000 / total_pop_2000) * 100)


# acs census clean
cln_2010 <- acs_cln_enh(acs_2010_data, year = 2010)
cln_2011 <- acs_cln_enh(acs_2011_data, year = 2011)
cln_2012 <- acs_cln_enh(acs_2012_data, year = 2012)
cln_2013 <- acs_cln_enh(acs_2013_data, year = 2013)
cln_2014 <- acs_cln_enh(acs_2014_data, year = 2014)
cln_2015 <- acs_cln_enh(acs_2015_data, year = 2015)
cln_2016 <- acs_cln_enh(acs_2016_data, year = 2016)
cln_2017 <- acs_cln_enh(acs_2017_data, year = 2017)


# Inflation 2000-2017 2.099%
cln_all <- cln_2000 %>%
  full_join(cln_2010, by = c("state", "county", "tract")) %>%
  full_join(cln_2017, by = c("state", "county", "tract")) %>%
  mutate(median_income_2000_adj = median_income_2000 * 1.02099^17,
         median_home_value_2000_adj = median_home_value_2000 * 1.02099^17) %>%
  mutate(income_chg_00_10 = median_income_2010 - median_income_2000_adj,
         income_chg_00_17 = median_income_2017 - median_income_2000_adj,
         home_value_chg_00_10 = median_home_value_2010 - 
           median_home_value_2000_adj,
         home_value_chg_00_17 = median_home_value_2017 - 
           median_home_value_2000_adj) %>%
  mutate(in_gl_00_10 = 
           case_when(
             income_chg_00_10 > 0 ~ "gain",
             income_chg_00_10 < 0 ~ "loss"
           ),
         in_gl_00_17 = 
           case_when(
             income_chg_00_17 > 0 ~ "gain",
             income_chg_00_17 < 0 ~ "loss"
           )
  )

dbWriteTable(defaultdb, "cln_census_2000_2017", cln_all, overwrite = TRUE,
             row.names = FALSE)

