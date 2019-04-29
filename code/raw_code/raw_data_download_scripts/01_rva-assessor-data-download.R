###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: Code to download data from Richmond's Assesor's Office             
##                                                                           
##                                                                           
##                                                                           
###############################################################################

# Download Zip file containing data-sets.
zipfile_url <- 
  "ftp://ftp.ci.richmond.va.us/Assessor/Real%20Tables/COR%20Public%20Data%20Dec%205%202018.zip"

destname <- "../../data/temp_data/CORPublicDataDec052018.zip"
download.file(url = zipfile_url, destfile = destname)

# Unzip Downloaded file. 
unzip(destname, exdir = "../../data/temp_data")

# Download Data Dictionaries.
download_data_dicts <- function(url){
  split_string <- unlist(strsplit(url, "/"))
  file_name <- split_string[length(split_string)]
  dir_path <- "../../data/temp_data"
  dest_path <- paste0(dir_path, file_name)
  download.file(url = url, destfile = dest_path)
}

urls <- c("http://www.richmondgov.com/Assessor/documents/layoutRealMaster.pdf",
          "http://www.richmondgov.com/Assessor/documents/layoutRealLand.pdf",
          "http://www.richmondgov.com/Assessor/documents/layoutRealImprov.pdf")

for(i in urls){
  download_data_dicts(i)
}




