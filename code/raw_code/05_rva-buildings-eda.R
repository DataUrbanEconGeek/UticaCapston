###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(dplyr)
library(ggplot2)
library(ggmap)
source("helper00_project-db-connection.R")

# Load in master building data frame from Data Warehouse.
m_buildings_df <- dbGetQuery(defaultdb, "SELECT *
                        from master_buildings")

# Create a frequency table by years.
year_freq <- m_buildings_df %>%
  group_by(YrBuilt) %>%
  count(YrBuilt) %>%
  arrange(desc(n))

# Plot out the top 25 years buildings were built in.
year_freq_plot <- ggplot(year_freq[1:25,], aes(x = reorder(as.factor(YrBuilt), 
                                                           n), y = n)) +
  geom_bar(stat="identity") +
  xlab("Year Built") +
  ylab("Number of Buildings") +
  theme_minimal() +
  coord_flip()

# Plot Time Series.
year_ts_plot <- ggplot(year_freq, aes(x = YrBuilt, y = n)) +
  geom_line()




