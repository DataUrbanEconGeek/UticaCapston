###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(censusapi)
library(dplyr)
source("../helper_scripts/helper00_project-db-connection.R")


apis <- listCensusApis()

acs_2010_meta <- listCensusMetadata("acs/acs5", 2010)
sf3_2000_meta <- listCensusMetadata("sf3", 2000)

#
rva_cbs_tracts <- pgGetGeom(spatialdb, "rva_cbs_census_tracts")
countys <- unique(rva_cbs_tracts$COUNTYFP)
regionin_string <- paste0("state:51+county:", paste0(countys, collapse = ","))

# P053001 - Median Household Income
# P010001 - Total Households
# H085001 - Median Home Value
# H001001 - Number of housing Units
# P037032 - Number of females with Bachelors Degree age 25+
# P037015 - Number of males with Bachelors Degree age 25+
# P001001 - Total Population
sf3_2000_data <- getCensus(name = "sf3",
                             vintage = 2000,
                             vars = c("P053001", "P010001", "H085001", 
                                      "H001001", "P037032", "P037015", 
                                      "P001001"),
                             region = "tract:*",
                             regionin = regionin_string)

dbWriteTable(defaultdb, "sf3_2000", sf3_2000_data, overwrite = TRUE,
             row.names = FALSE)

# B19013_001E - Median Houshold Income
# B25077_001E - Median Home Value
# B15002_032E - Number of females with Bachelors Degree age 25+
# B15002_015E - Number of males with Bachelors Degree age 25+
# B01003_001E - Total Population

acs5_extract <- function(year){
  df <- getCensus(name = "acs/acs5",
                 vintage = year, 
                 vars = c("B19013_001E", "B25077_001E",
                          "B15002_032E", "B15002_015E", "B01003_001E"), 
                 region = "tract:*",
                 regionin = regionin_string)
  return(df)
}

acs_2010_data <- acs5_extract(2010)

dbWriteTable(defaultdb, "acs5_2010", acs_2010_data, overwrite = TRUE, 
             row.names = FALSE)

acs_2011_data <- acs5_extract(2011)

dbWriteTable(defaultdb, "acs5_2011", acs_2011_data, overwrite = TRUE, 
             row.names = FALSE)

acs_2012_data <- acs5_extract(2012)

dbWriteTable(defaultdb, "acs5_2012", acs_2012_data, overwrite = TRUE, 
             row.names = FALSE)

acs_2013_data <- acs5_extract(2013)

dbWriteTable(defaultdb, "acs5_2013", acs_2013_data, overwrite = TRUE, 
             row.names = FALSE)

acs_2014_data <- acs5_extract(2014)

dbWriteTable(defaultdb, "acs5_2014", acs_2014_data, overwrite = TRUE, 
             row.names = FALSE)

acs_2015_data <- acs5_extract(2015)

dbWriteTable(defaultdb, "acs5_2015", acs_2015_data, overwrite = TRUE, 
             row.names = FALSE)

acs_2016_data <- acs5_extract(2016)

dbWriteTable(defaultdb, "acs5_2016", acs_2016_data, overwrite = TRUE, 
             row.names = FALSE)

acs_2017_data <- acs5_extract(2017)

dbWriteTable(defaultdb, "acs5_2017", acs_2017_data, overwrite = TRUE,
             row.names = FALSE)

