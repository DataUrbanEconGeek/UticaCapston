library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(stringi)
library(scales)
source("../helper_scripts/helper00_project-db-connection.R")

improve_df <- dbGetQuery(defaultdb, "SELECT * from cln_real_improvement")

scatter <- improve_df %>%
  select(NumRms, NumBdRms) %>%
  ggplot(aes(x = NumRms, y = NumBdRms)) +
  geom_point(color = "steelblue") +
  labs(title = "Number of Bedrooms vs Number of Rooms \nRichmond Properties",
       x = "Number of Rooms", y = "Number of Bedrooms") +
  theme_tufte() +
  #theme_minimal() +
  theme(panel.grid.major = element_line(color = "darkgrey", size = .25, 
                                        linetype = "solid")) +
  coord_cartesian(xlim = c(0, 400), ylim = c(0, 115))

ggsave(filename = "scatter.png", scatter)

histogram <- improve_df %>%
  select(NumRms) %>%
  ggplot(aes(x = NumRms)) +
  geom_histogram(breaks = seq(0, 372, by = 2), color = "steelblue") +
  labs(title = "Number of Rooms", x = "Number of Rooms") +
  theme_tufte()

ggsave(filename = "hist.png", histogram)

boxplot <- improve_df %>%
  select(NumRms, NumBdRms, Num2Baths) %>%
  gather("type", "units") %>%
  mutate(type = factor(type, 
                       levels = c("NumRms", "NumBdRms", "Num2Baths"))) %>%
  ggplot(aes(x = type, y = units, color = type)) +
  geom_boxplot(outlier.size = 0.5, size = 0.25) +
  labs(title = "Distribution of Room Types", x = "Room Type", 
       y = "Number of Units") +
  scale_color_manual(values = c("steelblue", "orange", "lightgray")) +
  scale_x_discrete(labels = c("Rooms", "Bedrooms", "2 Bath")) +
  theme_tufte() +
  theme(legend.position = "none", 
        panel.grid.major.y = element_line(color = "darkgrey", size = .25, 
                                        linetype = "solid"))

ggsave(filename = "boxplot.png", boxplot)

boxplot2 <- improve_df %>%
  select(NumRms, NumBdRms, Num2Baths) %>%
  gather("type", "units") %>%
  mutate(type = factor(type, 
                       levels = c("NumRms", "NumBdRms", "Num2Baths"))) %>%
  ggplot(aes(x = type, y = units, color = type)) +
  geom_boxplot(outlier.size = 0.5, size = 0.25) +
  labs(title = "Distribution of Room Types", x = "Room Type", 
       y = "Number of Units") +
  scale_color_manual(values = c("steelblue", "orange", "lightgray")) +
  scale_x_discrete(labels = c("Rooms", "Bedrooms", "2 Bath")) +
  scale_y_continuous(limits = c(0, 15)) +
  theme_tufte() +
  theme(legend.position = "none", 
        panel.grid.major.y = element_line(color = "darkgrey", size = .25, 
                                          linetype = "solid"))

ggsave(filename = "boxplot2.png", boxplot2)

barchart <- improve_df %>%
  select(CondDesc) %>%
  filter(CondDesc != "") %>%
  mutate(CondDesc = stri_trans_totitle(CondDesc)) %>%
  group_by(CondDesc) %>%
  count() %>%
  ggplot(aes(x = reorder(CondDesc, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(y = n, label = n), hjust = -0.05, size = 4.0) +
  coord_flip() +
  labs(title = "Number of Properties by Condition, Richmond, VA",
       x = "Building Condition", y = "Count") +
  scale_y_continuous(limits = c(0, 42000)) +
  theme_tufte() +
  theme(panel.grid.major.x = element_line(color = "darkgrey", size = .25, 
                                          linetype = "solid"))
  
ggsave(filename = "barchart.png", barchart, height = 11, width = 17, scale = .5)

piechart <- improve_df %>%
  select(CondDesc) %>%
  filter(CondDesc != "") %>%
  group_by(CondDesc) %>%
  count() %>%
  ungroup() %>%
  mutate(n = n/sum(n), CondDesc = stri_trans_totitle(CondDesc)) %>%
  ggplot(aes(x = "", y = n, fill = reorder(CondDesc, -n))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  labs(title = "Number of Properties by Condition, \n\t\tRichmond, VA",
       fill = "Condition Description") +
  theme_tufte() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_blank()) +
  geom_text(aes(label = c(paste0(sort(round(n*100), 
                                      decreasing = TRUE), "%")[1:4]
                          , "", "", "")[c(5,3,2,1,5,6,7)]), 
            position = position_stack(vjust = 0.5))

ggsave(filename = "piechart.png", piechart)

time_seriese <- improve_df %>%
  select(YrBuilt, Stories) %>%
  group_by(YrBuilt) %>%
  summarise(avg_stories = mean(na.omit(as.numeric(Stories)))) %>%
  ggplot(aes(x = YrBuilt, y = avg_stories)) +
  geom_line() +
  labs(title = "Average Stories by Year, Richmond, VA",
       y = "Average Number of Stories") +
  theme_tufte() +
  theme(panel.grid.major = element_line(color = "darkgrey", size = .25, 
                                        linetype = "solid")) +
  coord_cartesian(xlim = c(1700, 2019), ylim = c(0, 5)) +
  scale_x_continuous(name = "Year", breaks = seq(1700, 2019, by = 50))

ggsave(filename = "time_seriese.png", time_seriese)








