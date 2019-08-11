###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(RPostgreSQL)
library(aws.s3)
library(httr)
library(rvest)
source("../helper_scripts/helper00_project-db-connection.R")


# Load in master building data frame from Data Warehouse.
buildings_df <- dbGetQuery(defaultdb, "SELECT * from properties_w_neighborhoods")

# Extract the property pin numbers
pins <- buildings_df$PIN

# Establish base paths for scraping and saving destinations 
url_base <- "http://eservices.ci.richmond.va.us/applications/PropertySearch/Image.ashx?pin="
file_dst_bs <- "../../../data/temp_data/"


# Use a loop to scrape photos, and upload to S3 bucket
for(i in 36856:length(pins)){
  url_full <- paste0(url_base, pins[i])
  file_full_dst <- paste0(file_dst_bs, pins[i], ".jpeg")
  
  GET(url_full, write_disk(file_full_dst, overwrite = TRUE))
  
  put_object(file = file_full_dst, 
             bucket = "anrosa-capstone-bucket/property-imgs")
  
  file.remove(file_full_dst)
  print((i/length(pins) * 100))
}

not_loadded <- c("C0050861068", "W0001952013", "S0001475035")


