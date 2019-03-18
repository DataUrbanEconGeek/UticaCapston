url <- "https://usbuildingdata.blob.core.windows.net/usbuildings-v1-1/Virginia.zip"
destname <- "~/projects/r_dc_building_maps/Virginia.zip"

download.file(url = url, destfile = destname)

unzip(destname)