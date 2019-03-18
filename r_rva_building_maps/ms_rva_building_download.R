url <- "https://usbuildingdata.blob.core.windows.net/usbuildings-v1-1/Virginia.zip"
destname <- "~/projects/rva/Virginia.zip"

download.file(url = url, destfile = destname)

unzip(destname)