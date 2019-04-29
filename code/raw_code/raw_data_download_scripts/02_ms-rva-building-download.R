###############################################################################
## Author: Andrew Rosa                                                       ##
##                                                                           ##
## Notes: Code to download Microsoft's US Building Footprint data for        ##
## Viginia from github.                                                      ##
##                                                                           ##
##                                                                           ##
###############################################################################

url <- "https://usbuildingdata.blob.core.windows.net/usbuildings-v1-1/Virginia.zip"
destname <- "../../../data/temp_data/Virginia.zip"

download.file(url = url, destfile = destname)

unzip(destname)