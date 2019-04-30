###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                   
##                                                                           
##                                                                           
###############################################################################

source("../helper_scripts/helper00_project-db-connection.R")
source("../raw_data_download_scripts/01_rva-assessor-data-download.R")
source("../raw_data_load_scripts/01_rva-assessor-data-load.R")

assessor_data_download()
dl_all_dictionaries()
assessor_load_to_db()


