library(censusapi)
library(dplyr)
library(ggplot2)
library(plotly)
library(tigris)
library(stringi)
library(maptools)
library(rpostgis)
source("helper00_project-db-connection.R")


download.file(url = "https://www2.census.gov/acs/downloads/Core_Tables/2000/BaseTables3852000.csv",
                          destfile = "../../data/raw_data/base_3852000.csv")

acs_2000 <- read.csv(file = "../../data/raw_data/base_3852000.csv", 
                     stringsAsFactors = FALSE)

unlink("../../data/raw_data/base_3852000.csv")

download.file(url = "https://www2.census.gov/acs/downloads/Core_Tables/2000/BaseTables5002000.csv", 
              destfile = "../../data/raw_data/base_5002000.csv")
              
acs_2000 <- read.csv(file = "../../data/raw_data/base_5002000.csv", 
                     stringsAsFactors = FALSE)

unlink("../../data/raw_data/base_5002000.csv")

download.file(url = "https://www2.census.gov/acs/downloads/Core_Tables/2000/BaseTables0502000.csv", 
              destfile = "../../data/raw_data/base_0502000.csv")

acs_2000 <- read.csv(file = "../../data/raw_data/base_0502000.csv", 
                     stringsAsFactors = FALSE)

unlink("../../data/raw_data/base_0602000.csv")

apis <- listCensusApis()

acs_2010_meta <- listCensusMetadata("acs/acs5", 2010)
sf1_2000_meta <- listCensusMetadata("sf1", 2000)
sf3_2000_meta <- listCensusMetadata("sf3", 2000)

cnames <- c("median_income", "median_home_value", "females_bach_deg", 
            "males_bach_deg", "total_pop")

#
va_tracts <- tracts("VA", cb = TRUE)
cb <- core_based_statistical_areas(cb = TRUE)

rva_cbs <- cb[grepl("Richmond, VA", cb$NAME),]
rva_cbs_tracts <- va_tracts[rva_cbs,]

countys <- unique(rva_cbs_tracts$COUNTYFP)

# B19013_001E - Median Houshold Income
# B25077_001E - Median Home Value
# B15002_032E - Number of females with Bachelors Degree age 25+
# B15002_015E - Number of males with Bachelors Degree age 25+
# B01003_001E - Total Population
regionin_string <- paste0("state:51+county:", paste0(countys, collapse = ","))

acs_income <- getCensus(name = "acs/acs5",
                        vintage = 2010, 
                        vars = c("B19013_001E", "B25077_001E",
                                 "B15002_032E", "B15002_015E", "B01003_001E"), 
                        region = "tract:*",
                        regionin = regionin_string)

acs_income_2017 <- getCensus(name = "acs/acs5",
                        vintage = 2017, 
                        vars = c("B19013_001E", "B25077_001E",
                                 "B15002_032E", "B15002_015E", "B01003_001E"), 
                        region = "tract:*",
                        regionin = regionin_string)


acs_old_names <- c("B19013_001E", "B25077_001E", "B15002_032E", "B15002_015E", 
                   "B01003_001E")

cln_2010 <- acs_income %>%
  rename_at(vars(acs_old_names), ~ cnames) %>%
  mutate(total_bach_deg = females_bach_deg + males_bach_deg) %>%
  mutate(percent_bach_deg = (total_bach_deg/total_pop)*100)

cln_2017 <- acs_income_2017 %>%
  rename_at(vars(acs_old_names), ~ cnames) %>%
  mutate(total_bach_deg = females_bach_deg + males_bach_deg) %>%
  mutate(percent_bach_deg = (total_bach_deg/total_pop)*100)

# P053001 - Median Household Income
# H076001 - Median Home Value
# P037032 - Number of females with Bachelors Degree age 25+
# P037015 - Number of males with Bachelors Degree age 25+
# P001001 - Total Population
sf3_2000_income <- getCensus(name = "sf3",
                             vintage = 2000,
                             vars = c("P053001", "H076001", "P037032", 
                                      "P037015", "P001001"),
                             region = "tract:*",
                             regionin = regionin_string)


sf3_old_names <- c("P053001", "H076001", "P037032", "P037015", "P001001")

cln_2000 <- sf3_2000_income %>%
  rename_at(vars(sf3_old_names), ~ cnames) %>%
  mutate(total_bach_deg = females_bach_deg + males_bach_deg) %>%
  mutate(percent_bach_deg = (total_bach_deg/total_pop)*100)

new_col_names <-c("state", "county", "tract", "mi_2000", "mhv_2000", "fbd_2000",
                  "mbd_2000", "tp_2000", "tbd_2000", "pbd_2000", "mi_2010", 
                  "mhv_2010", "fbd_2010", "mbd_2010", "tp_2010", "tbd_2010", 
                  "pbd_2010", "mi_2017", "mhv_2017", "fbd_2017", "mbd_2017", 
                  "tp_2017", "tbd_2017", "pbd_2017")

# Inflation 2000-2017 2.099%
cln_all <- cln_2000 %>%
  full_join(cln_2010, by = c("state", "county", "tract")) %>%
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



url_tract_relationship <- "https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/va51trf.txt"

dest_file <- "../../data/raw_data/tract_2000_2010_relations"
download.file(url_tract_relationship, 
              destfile = dest_file)

tract_relations <- read.table(dest_file, sep = ",", colClasses = "character")

rva_tract_relations <- tract_relations %>%
  select(V1, V2, V4, V13) %>%
  filter(V1 == "51" & V2 == "760")

gent_tests <- cln_all %>%
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


rva_gent <- gent_tests %>%
  filter(county == "760")

income_gain_loss <- rva_gent %>%
  ggplot(aes(x = mi_2000_adj, y = mi_2017)) +
  geom_smooth(method = "lm", se = FALSE, color = "magenta") +
  geom_point(aes(color = in_chg, text = tract)) +
  labs(title = "Richmond Virginia: \nChange in Median Household Income 2000-2017 \nAdjusted for Inflation",
       x = "2000 Median Household Income Adjusted for Inflations",
       y = "2017 Median Household Income",
       color = "Change in \nMedian Income") +
  theme_minimal()

Sys.setenv("plotly_username"="dataecongeek")
Sys.setenv("plotly_api_key"="Sa4JYdmxdsK84U55109N")
median_income <- ggplotly(income_gain_loss, tooltip = c("tract", "in_chg"))

api_create(median_income, filename = "change_in_income")

#
rva_tracts <- pgGetGeom(spatialdb, "rva_census_tracts")
rva_tracts <- fortify(rva_tracts, region = "TRACTCE")


rva_tracts_n_gent <- rva_tracts %>%
  inner_join(rva_gent, by = c("id" = "tract"))
  
ggplot(rva_tracts_n_gent, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gentrified, color = gentrified), alpha = 0.5) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())
  



