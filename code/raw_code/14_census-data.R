library(censusapi)
library(dplyr)

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


# B19013_001E - Median Houshold Income
# B25077_001E - Median Home Value
# B15002_032E - Number of females with Bachelors Degree age 25+
# B15002_015E - Number of males with Bachelors Degree age 25+
# B01003_001E - Total Population
acs_income <- getCensus(name = "acs/acs5",
                        vintage = 2010, 
                        vars = c("NAME", "TRACT", "B19013_001E", "B25077_001E",
                                 "B15002_032E", "B15002_015E", "B01003_001E"), 
                        region = "tract:*",
                        regionin = "state:51+county:760")

acs_income_2017 <- getCensus(name = "acs/acs5",
                        vintage = 2017, 
                        vars = c("NAME", "TRACT", "B19013_001E", "B25077_001E",
                                 "B15002_032E", "B15002_015E", "B01003_001E"), 
                        region = "tract:*",
                        regionin = "state:51+county:760")


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
                             regionin = "state:51+county:760")

cnames <- c("median_income", "median_home_value", "females_bach_deg", 
            "males_bach_deg", "total_pop")

sf3_old_names <- c("P053001", "H076001", "P037032", "P037015", "P001001")

cln_2000 <- sf3_2000_income %>%
  rename_at(vars(sf3_old_names), ~ cnames) %>%
  mutate(total_bach_deg = females_bach_deg + males_bach_deg) %>%
  mutate(percent_bach_deg = (total_bach_deg/total_pop)*100)




