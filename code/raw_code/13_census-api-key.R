###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(acs)
key <- "61537b6f200c32b6b026d0e5784d070eb2594e4f"

# For use with acs package
api.key.install(key = key)

# For use with censusapi package
# Add key to .Renviron
Sys.setenv(CENSUS_KEY = key)
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

