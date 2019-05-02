###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
source("../helper_scripts/helper00_project-db-connection.R")

cln_all <- dbGetQuery(defaultdb, "SELECT * from cln_census_2000_2017")


gent_tests <- cln_all %>%
  filter(median_income_2017 != -666666666 & total_pop_2000 > 500) %>%
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
  ) %>%
  mutate(change_pbd_00_10 = percent_bach_deg_2010 - percent_bach_deg_2000,
         change_pbd_00_17 = percent_bach_deg_2017 - percent_bach_deg_2000) %>%
  mutate(
    gentrified_10 = case_when(
      change_pbd_00_10 >= quantile(na.omit(change_pbd_00_10), 0.6) & 
        home_value_chg_00_10 >= quantile(na.omit(home_value_chg_00_10), 0.6) &
        eligibil_for_gentrification == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    gentrified_17 = case_when(
      change_pbd_00_17 >= quantile(na.omit(change_pbd_00_17), 0.6) & 
        home_value_chg_00_17 >= quantile(na.omit(home_value_chg_00_17), 0.6) &
        eligibil_for_gentrification == "yes" ~ "yes",
      TRUE ~ "no"
    )
  )

dbWriteTable(defaultdb, "cbsa_gent_2000_2017", gent_tests, overwrite = TRUE,
             row.names = FALSE)

rva_gent <- gent_tests %>%
  filter(county == "760")

dbWriteTable(defaultdb, "rva_gent_2000_2017", rva_gent, overwrite = TRUE,
             row.names = FALSE)
