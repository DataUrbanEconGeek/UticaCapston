###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
source("../helper_scripts/helper00_project-db-connection.R")


eligibility_tests <- function(x){
  df <- x %>%
    mutate(
      eligibility_test_1 = case_when(
        median_income_2000 <= quantile(na.omit(median_income_2000), 0.4) ~ 1,
        TRUE ~ 0
        ),
      eligibility_test_2 = case_when(
        median_home_value_2000 <= quantile(na.omit(median_home_value_2000), 
                                           0.4) ~ 1,
        TRUE ~ 0
        )
      ) %>%
    mutate(
      eligibil_for_gentrification = case_when(
        eligibility_test_1 + eligibility_test_2 == 2 ~ "yes",
        TRUE ~ "no"
        )
      )
  return(df)
}

bachelors_change <- function(x, year){
  pbd_latter <- paste0("percent_bach_deg_", year)
  var_name <- paste0("change_pbd_00_", str_sub(as.character(year), 3, 4))
  expre <- paste0(pbd_latter, " - percent_bach_deg_2000")
  df <- x %>%
    mutate_(change_pbd = expre)
  colnames(df)[length(names(df))] <- var_name
  return(df)
}

gentrification_tests <- function(x, year){
  cpbd_var_name <- paste0("change_pbd_00_", str_sub(as.character(year), 3, 4))
  hvc_var_name <- paste0("home_value_chg_00_", str_sub(as.character(year), 3, 4))
  new_var_name <- paste0("gentrified_", str_sub(as.character(year), 3, 4))
  cpbd_var <- x[, grep(cpbd_var_name, names(x))]
  hvc_var <- x[, grep(hvc_var_name, names(x))]
  new_var <- x[, grep(new_var_name, names(x))]
  gent_test1 <- list(
    quo(cpbd_var >= quantile(na.omit(cpbd_var), 0.6) ~ "yes"),
    quo(TRUE ~ "no")
  )
  gent_test2 <- list(
    quo(hvc_var >= quantile(na.omit(hvc_var), 0.6) ~ "yes"),
    quo(TRUE ~ "no")
  )
  gent_test3 <- list(
    quo(eligibil_for_gentrification == "yes" & gentrified_t1 == "yes" &
          gentrified_t2 == "yes" ~ "yes"),
    quo(TRUE ~ "no")
  )
  df <- x %>%
    mutate(
    gentrified_t1 = case_when(!!!gent_test1),
    gentrified_t2 = case_when(!!!gent_test2),
    gentrified_t3 = case_when(!!!gent_test3)
  )
}

# Load data sets
for(i in 2010:2017){
  query_string <- paste0("SELECT * from cln_census_2000_", i)
  df_name <- paste0("cln_2000_", i)
  temp_df <- dbGetQuery(defaultdb, query_string)
  assign(df_name, temp_df)
}

# Run tests
for(i in 2010:2017){
  retrieve <- paste0("cln_2000_", i)
  df_name <- paste0("gent_2000_", i)
  f_var1 <- paste0("median_income_", i)
  f_var2 <- paste0("median_home_value_", i)
  expre <- paste0(f_var1, " != -666666666 & ", f_var2, " != -666666666")
  temp_df <- get(retrieve) %>%
    filter(total_pop_2000 > 500) %>%
    filter_(expre) %>%
    eligibility_tests() %>%
    bachelors_change(i) %>%
    gentrification_tests(i)
  assign(df_name, temp_df)
  rm(list = retrieve)
}

for(i in 2010:2017){
  retrieve <- paste0("gent_2000_", i)
  df_name <- paste0("cbsa_gent_2000_", i)
  dbWriteTable(defaultdb, df_name, get(retrieve), overwrite = TRUE,
               row.names = FALSE)
}

for(i in 2010:2017){
  retrieve <- paste0("gent_2000_", i)
  df_name <- paste0("rva_gent_", i)
  temp_df <- get(retrieve) %>%
    filter(county == "760")
  assign(df_name, temp_df)
}

for(i in 2010:2017){
  retrieve <- paste0("rva_gent_", i)
  df_name <- paste0("rva_gent_2000_", i)
  dbWriteTable(defaultdb, df_name, get(retrieve), overwrite = TRUE,
               row.names = FALSE)
}

for(i in 2010:2017){
  retrieve <- paste0("rva_gent_", i)
  df_name <- paste0("test_results_", i)
  new_var_name <- paste0("gentrified_", i)
  temp_df <- get(retrieve) %>%
    select(state, county, tract, eligibil_for_gentrification, gentrified_t3) %>%
    mutate(!!new_var_name := gentrified_t3) %>%
    select(-gentrified_t3)
  assign(df_name, temp_df)
}

gent_test_results <- test_results_2010

for(i in 2011:2017){
  retrieve <- paste0("test_results_", i)
  gent_test_results <- get(retrieve) %>%
    full_join(gent_test_results, by = c("state", "county", "tract", 
                                        "eligibil_for_gentrification"))
}

gent_test_results <- gent_test_results %>%
  mutate(
    binary_test_10 = case_when(
      gentrified_2010 == "yes" ~ 1,
      TRUE ~ 0
      ),
    binary_test_11 = case_when(
      gentrified_2011 == "yes" ~ 1,
      TRUE ~ 0
      ),
    binary_test_12 = case_when(
      gentrified_2012 == "yes" ~ 1,
      TRUE ~ 0
      ),
    binary_test_13 = case_when(
      gentrified_2013 == "yes" ~ 1,
      TRUE ~ 0
      ),
    binary_test_14 = case_when(
      gentrified_2014 == "yes" ~ 1,
      TRUE ~ 0
      ),
    binary_test_15 = case_when(
      gentrified_2015 == "yes" ~ 1,
      TRUE ~ 0
      ),
    binary_test_16 = case_when(
      gentrified_2016 == "yes" ~ 1,
      TRUE ~ 0),
    binary_test_17 = case_when(
      gentrified_2017 == "yes" ~ 1,
      TRUE ~ 0
      )
    ) %>%
  mutate(likely_gentrified_prob = (binary_test_10 + binary_test_11 +
           binary_test_12 + binary_test_13 + binary_test_14 + binary_test_15 +
           binary_test_16 + binary_test_17) / 8) %>%
  select(state, county, tract, eligibil_for_gentrification, 
         likely_gentrified_prob) %>%
  mutate(
    gentrified = case_when(
      likely_gentrified_prob > 0.5 ~ "yes",
      TRUE ~ "no"
    ))

dbWriteTable(defaultdb, "rva_gentrification_results", gent_test_results,
             overwrite = TRUE, row.names = FALSE)
