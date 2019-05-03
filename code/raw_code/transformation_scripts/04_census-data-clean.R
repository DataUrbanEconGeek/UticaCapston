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

for(i in 2010:2017){
  query_string <- paste0("SELECT * from acs5_", i)
  df_name <- paste0("acs_", i, "_data")
  temp_df <- dbGetQuery(defaultdb, query_string)
  assign(df_name, temp_df)
}

# 2000 sf3 census clean
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
for(i in 2010:2017){
  df_name <- paste0("cln_", i)
  retirieve <- paste0("acs_", i, "_data")
  temp_df <- acs_cln_enh(get(retirieve), year = i)
  assign(df_name, temp_df)
  rm(list = retirieve)
}

# Census Join
census_full_join <- function(x, y){
  full_join(x = x, y = y, by = c("state", "county", "tract"))
}

census_all <- cln_2000

for(i in 2010:2017){
  retrieve <- paste0("cln_", i)
  census_all <- census_all %>%
    census_full_join(get(retrieve))
}

# Inflation rates
cpi_2000 <- 172.2
cpis_2010_2017 <- list(c(2010, 0.02099) )

census_all_inflation <- census_all %>%
  mutate(median_income_2000_adj2017 = median_income_2000 * 1.02099^17,
         median_home_value_2000_adj2017 = median_home_value_2000 * 1.02099^17)

# Inflation 2000-2017 2.099%
cln_all <- census_all %>%
  mutate(median_income_2000_adj = median_income_2000 * 1.02099^17,
         median_home_value_2000_adj = median_home_value_2000 * 1.02099^17) %>%
  mutate(income_chg_00_10 = median_income_2010 - median_income_2000_adj,
         income_chg_00_11 = median_income_2011 - median_income_2000_adj,
         income_chg_00_12 = median_income_2012 - median_income_2000_adj,
         income_chg_00_13 = median_income_2013 - median_income_2000_adj,
         income_chg_00_14 = median_income_2014 - median_income_2000_adj,
         income_chg_00_15 = median_income_2015 - median_income_2000_adj,
         income_chg_00_16 = median_income_2016 - median_income_2000_adj,
         income_chg_00_17 = median_income_2017 - median_income_2000_adj,
         home_value_chg_00_10 = median_home_value_2010 - 
           median_home_value_2000_adj,
         home_value_chg_00_11 = median_home_value_2011 - 
           median_home_value_2000_adj,
         home_value_chg_00_12 = median_home_value_2012 - 
           median_home_value_2000_adj,
         home_value_chg_00_13 = median_home_value_2013 - 
           median_home_value_2000_adj,
         home_value_chg_00_14 = median_home_value_2014 - 
           median_home_value_2000_adj,
         home_value_chg_00_15 = median_home_value_2015 - 
           median_home_value_2000_adj,
         home_value_chg_00_16 = median_home_value_2016 - 
           median_home_value_2000_adj,
         home_value_chg_00_17 = median_home_value_2017 - 
           median_home_value_2000_adj) %>%
  mutate(in_gl_00_10 = 
           case_when(
             income_chg_00_10 > 0 ~ "gain",
             income_chg_00_10 < 0 ~ "loss"
           ),
         in_gl_00_11 = 
           case_when(
             income_chg_00_11 > 0 ~ "gain",
             income_chg_00_11 < 0 ~ "loss"
           ),
         in_gl_00_12 = 
           case_when(
             income_chg_00_12 > 0 ~ "gain",
             income_chg_00_12 < 0 ~ "loss"
           ),
         in_gl_00_13 = 
           case_when(
             income_chg_00_13 > 0 ~ "gain",
             income_chg_00_13 < 0 ~ "loss"
           ),
         in_gl_00_14 = 
           case_when(
             income_chg_00_14 > 0 ~ "gain",
             income_chg_00_14 < 0 ~ "loss"
           ),
         in_gl_00_15 = 
           case_when(
             income_chg_00_15 > 0 ~ "gain",
             income_chg_00_15 < 0 ~ "loss"
           ),
         in_gl_00_16 = 
           case_when(
             income_chg_00_16 > 0 ~ "gain",
             income_chg_00_16 < 0 ~ "loss"
           ),
         in_gl_00_17 = 
           case_when(
             income_chg_00_17 > 0 ~ "gain",
             income_chg_00_17 < 0 ~ "loss"
           )
  )

dbWriteTable(defaultdb, "cln_census_2000_2017", cln_all, overwrite = TRUE,
             row.names = FALSE)

