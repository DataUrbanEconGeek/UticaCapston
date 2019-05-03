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
  perc_bach_deg_exp <- paste0("(", acs_new_names[3], " / total_bach_deg) * 100")
  df_cln <- df %>%
    rename_at(vars(acs_old_names), ~ acs_new_names) %>%
    mutate_(total_bach_deg = tot_bach_deg_exp) %>%
    mutate_(percent_bach_deg = perc_bach_deg_exp)
  colnames(df_cln)[9] <- paste0("total_bach_deg", "_", year)
  colnames(df_cln)[10] <- paste0("percent_bach_deg", "_", year)
  return(df_cln)
} 


# Load data
cpis <- dbGetQuery(defaultdb, "SELECT * from cpi_2000_2017")

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
mutate_adj_for_inflation <- function(x, year){
  cpi_2000 <- 172.2
  year_var <- enquo(year)
  var_name1 <- paste0("median_income_2000_adj", year)
  var_name2 <- paste0("median_home_value_2000_adj", year)
  cpi_for_year <- filter(cpis, year == !!year_var)[,2]
  mutate(x, !!var_name1 := (cpi_for_year / cpi_2000) * median_income_2000,
         !!var_name2 := (cpi_for_year / cpi_2000) * median_home_value_2000)
}

census_all_inflation <- census_all 

for(i in 2010:2017){
  census_all_inflation <- census_all_inflation %>%
    mutate_adj_for_inflation(i)
}
  

# changes in income and home value
mutate_changes <- function(x, year){
  year_char <- as.character(year)
  upper_var1 <- paste0("median_income_", year)
  lower_var1 <- paste0("median_income_2000_adj", year)
  upper_var2 <- paste0("median_home_value_", year)
  lower_var2 <- paste0("median_home_value_2000_adj", year)
  expre1 <- paste0(upper_var1, " - ", lower_var1)
  expre2 <- paste0(upper_var2, " - ", lower_var2)
  var_name1 <- paste0("income_chg_00_", str_sub(year_char, 3, 4))
  var_name2 <- paste0("home_value_chg_00_", str_sub(year_char, 3, 4))
  df <- mutate_(x, var_name1 = expre1, var_name2 = expre2)
  colnames(df)[length(names(df)) - 1] <- var_name1
  colnames(df)[length(names(df))] <- var_name2
  return(df)
}

census_changes <- census_all_inflation

for(i in 2010:2017){
  census_changes <- census_changes %>%
    mutate_changes(i)
}

# income gain or loss
cln_all <- census_changes %>%
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

