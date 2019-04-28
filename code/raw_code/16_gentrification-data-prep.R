###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
source("helper00_project-db-connection.R")

cln_all <- dbGetQuery(defaultdb, "SELECT * from cln_census_2000_2017")


gent_tests <- cln_all %>%
  filter(mi_2017 != -666666666 & tp_2000 > 500) %>%
  mutate(
    eligibility_test_1 = case_when(
      mi_2000 <= quantile(na.omit(mi_2000), 0.4) ~ 1,
      TRUE ~ 0
    ),
    eligibility_test_2 = case_when(
      mhv_2000 <= quantile(na.omit(mhv_2000), 0.4) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    eligibil_for_gentrification = case_when(
      eligibility_test_1 + eligibility_test_2 == 2 ~ "yes",
      TRUE ~ "no"
    )
  ) %>%
  mutate(change_pbd = pbd_2017 - pbd_2000) %>%
  mutate(
    gentrified = case_when(
      change_pbd >= quantile(na.omit(change_pbd), 0.6) & 
        mhv_chg >= quantile(na.omit(mhv_chg), 0.6) &
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
