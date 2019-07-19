###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(dplyr)
library(stringr)
source("helper00_project-db-connection.R")

dbGetQuery(defaultdb, "SELECT * FROM pg_catalog.pg_tables WHERE 
           schemaname = 'public'") 

improve_df <- dbGetQuery(defaultdb, "SELECT * from real_improvement")

improve_df$PIN <- trimws(improve_df$PIN)
improve_df$ImprType <- as.factor(trimws(improve_df$ImprType))
improve_df$UseDesc <- as.factor(trimws(improve_df$UseDesc))
improve_df$BldgType <- trimws(improve_df$BldgType)
improve_df$Foundat <- as.factor(trimws(improve_df$Foundat))
improve_df$ConstFr <- as.factor(trimws(improve_df$ConstFr))
improve_df$ExtDesc1 <- as.factor(trimws(improve_df$ExtDesc1))
improve_df$ExtDesc2 <- as.factor(trimws(improve_df$ExtDesc2))
improve_df$CondCode <- as.factor(trimws(improve_df$CondCode))
improve_df$CondDesc <- as.factor(trimws(improve_df$CondDesc))
improve_df$RoofDesc <- as.factor(trimws(improve_df$RoofDesc))
improve_df$RoofMatD <- as.factor(trimws(improve_df$RoofMatD))
improve_df$HeatDesc <- trimws(improve_df$HeatDesc)
improve_df$WCovDes1 <- as.factor(trimws(improve_df$WCovDes1))
improve_df$FCovDes1 <- as.factor(trimws(improve_df$FCovDes1))
improve_df$FCovDes2 <- as.factor(trimws(improve_df$FCovDes2))
improve_df$FCovDes2 <- as.factor(improve_df$CentrlAC)

st2_fix <- c("W0000948030", "N0000208020", "N0000596019", "N0000864031", 
             "N0000120006")

st1_5_fix <- c("W0200114028")

#
improve_df_feat_add <- improve_df %>%
  mutate(
    time_range = case_when(
      YrBuilt < 1840 ~ "Before 1840",
      YrBuilt >= 1840 & YrBuilt < 1880 ~ "1840-1879",
      YrBuilt >= 1880 & YrBuilt < 1900 ~ "1880-1899",
      YrBuilt >= 1900 & YrBuilt < 1920 ~ "1900-1919",
      YrBuilt >= 1920 & YrBuilt < 1940 ~ "1920-1939",
      YrBuilt >= 1940 & YrBuilt < 1960 ~ "1940-1959",
      YrBuilt >= 1960 & YrBuilt < 1980 ~ "1960-1979",
      YrBuilt >= 1980 & YrBuilt < 2000 ~ "1980-1999",
      YrBuilt >= 2000 & YrBuilt < 2020 ~ "2000-2019",
      TRUE ~ "Unknown"
    )
  ) %>%
  mutate(Stories = replace(Stories, PIN %in% st2_fix, 2)) %>%
  mutate(Stories = replace(Stories, PIN %in% st1_5_fix, 1.5)) %>%
  mutate(Stories = replace(Stories, 
                           grepl("2sty", BldgType) & ImprType == "DWELLING",
                           2)) %>%
  mutate(Stories = replace(Stories, 
                           grepl("2 sty", BldgType) & ImprType == "DWELLING", 
                           2)) %>%
  mutate(Stories = replace(Stories, 
                           grepl("2.5 sty", BldgType) & ImprType == "DWELLING", 
                           2)) %>%
  mutate(Stories = replace(Stories, 
                           grepl("Duplex", BldgType) & ImprType == "DWELLING", 
                           2)) %>%
  mutate(Stories = replace(Stories, 
                           grepl("Splits", BldgType) & ImprType == "DWELLING", 
                           1.5)) %>%
  mutate(Stories = replace(Stories, 
                           grepl("1.5", BldgType) & ImprType == "DWELLING", 
                           1.5)) %>%
  mutate(Stories = replace(Stories, 
                           grepl("1sty", BldgType) & ImprType == "DWELLING", 
                           1)) %>%
  mutate(Stories_fixed = as.numeric(gsub("\\.{2}", ".", gsub(",", ".", 
                                    trimws(str_remove(Stories, "\\+"), "right")
  )))) %>%
  mutate(
    stories_new = case_when(
      Stories_fixed < 2 ~ "Less than 2 stories",
      Stories_fixed >= 2 & Stories_fixed < 3 ~ "2-3 stories",
      Stories_fixed >= 3 & Stories_fixed < 4 ~ "3-4 stories",
      Stories_fixed >= 4 & Stories_fixed < 5 ~ "4-5 stories",
      Stories_fixed >= 5 ~ "5 or more stories",
      TRUE ~ "NA"
    )
  )

dbWriteTable(defaultdb, "cln_real_improvement", improve_df_feat_add[2:86], 
             overwrite = TRUE)

res <- c("Single family", "Duplex", "Townhs-owner", "Condo-HRise", "Condo-Dplx",
         "Multi-family", "4-6 family", "1 family-owner", "Other residential",
         "Row-type", "Condo-Trplx", "1 family-tenant", "1 family-other", 
         "Townhs-tenant", "Apartments - Multiple Res", "Apartments - High Rise",
         "School Dormitory", "Residential Garage", "Rooming House",
         "Multiple Res-Retirement Com", "Senior Care Facility", 
         "Multiple Res Senior-Low Rise", "Residence - Single Family ",
         "Multiple Res-Low Rise, Shell", "Loft Warehouse", 
         "Apartment, High Rise, Shell", "Multiple Res-Assisted Living",
         "Home For The Elderly")

residentail <- improve_df_feat_add %>%
  filter(UseDesc %in% res)


dwellings <- improve_df_feat_add %>%
  filter(ImprType == "DWELLING")

commercial <- improve_df_feat_add %>%
  filter(ImprType == "COMMERCIAL")


d2 <- dwellings %>%
  arrange(YrBuilt) %>%
  group_by(PIN) %>%
  filter(row_number() == 1)

single_fam <- d2 %>%
  filter(UseDesc == "Single family" & Stories_fixed <= 2) %>%
  filter(FinSize > 2)

under_100sf <- single_fam %>%
  filter(FinSize <= 100)

agg_single_fam <- single_fam %>%
  select(YrBuilt, FinSize) %>%
  filter(YrBuilt != is.na(YrBuilt) & YrBuilt != 2019) %>%
  group_by(YrBuilt) %>%
  summarise(median_sf = median(FinSize))

library(ggplot2)

ggplot(agg_single_fam, aes(x = YrBuilt, y = median_sf)) +
  geom_line()


single_fam %>%
  filter(Stories_fixed > 0 & FinSize < 15000 & Stories_fixed != 1.51) %>%
  ggplot(aes(y = FinSize, x = as.factor(Stories_fixed))) +
  geom_boxplot()


ave_stories_year <- improve_df_feat_add %>%
  select(YrBuilt, Stories) %>%
  group_by(YrBuilt) %>%
  summarise(avg_stories = mean(na.omit(as.numeric(Stories))))

write.csv(x = ave_stories_year, file = "../../../data/props_stories.csv")


