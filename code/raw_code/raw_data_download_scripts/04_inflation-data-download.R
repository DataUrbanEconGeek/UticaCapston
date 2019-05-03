###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

# CPI - Consumer Price Index

url <-"https://download.bls.gov/pub/time.series/cu/cu.data.0.Current"

dest_file <- "../../../data/raw_data/cpi.txt"

download.file(url = url, destfile = dest_file)

