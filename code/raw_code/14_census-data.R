library(censusapi)
library(dplyr)
source("helper00_project-db-connection.R")


apis <- listCensusApis()

acs_2010_meta <- listCensusMetadata("acs/acs5", 2010)
sf1_2000_meta <- listCensusMetadata("sf1", 2000)
sf3_2000_meta <- listCensusMetadata("sf3", 2000)

#
rva_cbs_tracts <- pgGetGeom(spatialdb, "rva_cbs_census_tracts")
countys <- unique(rva_cbs_tracts$COUNTYFP)

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

dbWriteTable(defaultdb, "sf3_2000", sf3_2000_income, overwrite = TRUE,
             row.names = FALSE)

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

dbWriteTable(defaultdb, "acs5_2010", acs_income, overwrite = TRUE, 
             row.names = FALSE)

acs_income_2017 <- getCensus(name = "acs/acs5",
                        vintage = 2017, 
                        vars = c("B19013_001E", "B25077_001E",
                                 "B15002_032E", "B15002_015E", "B01003_001E"), 
                        region = "tract:*",
                        regionin = regionin_string)

dbWriteTable(defaultdb, "acs5_2017", acs_income_2017, overwrite = TRUE,
             row.names = FALSE)

