###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes: 
##                                                                           
###############################################################################

library(dplyr)
library(ggplot2)
library(plotly)
source("../helpers_scripts/helper00_project-db-connection.R")
source("../helper_scripts/helper04_color_palette.R")

rva_gent <- dbGetQuery(defaultdb, "SELECT * from rva_gent_2000_2017")

income_gain_loss <- rva_gent %>%
  ggplot(aes(x = mi_2000_adj, y = mi_2017)) +
  geom_smooth(method = "lm", se = FALSE, color = proj_palette[1]) +
  geom_point(aes(color = in_chg, text = tract)) +
  scale_color_gradient(low = proj_palette[19], high = proj_palette[3]) +
  labs(title = "Richmond Virginia: \nChange in Median Household Income 2000-2017 \nAdjusted for Inflation",
       x = "2000 Median Household Income Adjusted for Inflations",
       y = "2017 Median Household Income",
       color = "Change in \nMedian Income") +
  theme_minimal()

Sys.setenv("plotly_username"="dataecongeek")
Sys.setenv("plotly_api_key"="Sa4JYdmxdsK84U55109N")
median_income <- ggplotly(income_gain_loss, tooltip = c("tract", "in_chg"))

api_create(median_income, filename = "change_in_income")
