###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(RPostgreSQL)
library(dplyr)
library(purrr)
library(glue)
library(stringr)
source("../helper_scripts/helper00_project-db-connection.R")

# Functions
remove_outliers <- function(df, num_var, group_var){
  quo_num_var <- enquo(num_var)
  quo_group_var <- enquo(group_var)
  
  p <- c(.25, .75)
  p_names <- map_chr(p, ~paste0("q_", .x*100))
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  
  stats <- df %>%
    group_by(!!quo_group_var) %>%
    summarize_at(vars(!!quo_num_var), funs(!!!p_funs)) %>%
    mutate(IQR = q_75 - q_25) %>%
    mutate(ub = q_75 + IQR,
           lb = q_25 - IQR) %>%
    select(!!quo_group_var, ub, lb)
  
  list_of_conditions <- c()
  
  for(i in 1:length(as.data.frame(stats)[, 1])){
    text1 <- as.data.frame(stats)[i, 1]
    text2 <- as.data.frame(stats)[i, 2]
    text3 <- as.data.frame(stats)[i, 3]
    condition <- quo(!!quo_group_var == !!rlang::eval_tidy(text1) & 
                       !!quo_num_var <= !!rlang::eval_tidy(text2) & 
                       !!quo_num_var >= !!rlang::eval_tidy(text3) ~ "keep")
    list_of_conditions <- c(list_of_conditions, condition) 
  }
  
  
  final_conditions <- c(list_of_conditions,
                        quo(TRUE ~ "discard"))
  
  
  df %>%
    mutate(
      filter_cond = case_when(!!!final_conditions)
      ) %>%
    filter(filter_cond == "keep") %>%
    select(-filter_cond)
    
}


# Load in building data with gentrification classifications frame from DB.
buildings_df <- dbGetQuery(defaultdb, "SELECT * 
                                          from properties_w_neighborhoods")

# Number of Stories Boxplot outliers removed
buildings_df %>%
  remove_outliers(num_var = stories_fixed, group_var = gent_n_eligible) %>%
  ggplot(aes(x = gent_n_eligible, y = stories_fixed, fill = gent_n_eligible)) +
  geom_boxplot()

# Age Boxplot
age_bp <- ggplot(buildings_df, aes(y = age_in_years, x = gent_n_eligible,
                                   fill = gent_n_eligible)) +
  geom_boxplot()

# Number of Stories Boxplot
stories_bp <- ggplot(buildings_df, aes(y = stories_fixed, x = gent_n_eligible,
                            fill = gent_n_eligible)) + 
  geom_boxplot()

# Age Boxplot neighborhood level
buildings_df %>%
  select(neighborhood, age_in_years, gent_n_eligible) %>%
  group_by(neighborhood, gent_n_eligible) %>%
  summarise(median_prop_age = median(age_in_years)) %>%
  ggplot(aes(x = gent_n_eligible, y = median_prop_age, 
             fill = gent_n_eligible)) +
  geom_boxplot()
  
#
buildings_df %>%
  select(age_in_years, stories_fixed, gent_n_eligible, neighborhood) %>%
  group_by(neighborhood, gent_n_eligible) %>%
  summarise(median_prop_age = median(age_in_years),
            median_stories = median(stories_fixed, na.rm = TRUE)) %>%
  ggplot(aes(x = median_prop_age, y = median_stories, color = gent_n_eligible)) +
  geom_point()
  
  


#
church_hill <- buildings_df %>%
  filter(neighborhood == "Church Hill")

ggplot(church_hill, aes(x = YrBuilt)) +
  geom_histogram() +
  geom_vline(xintercept = median(church_hill$YrBuilt), color = "red") +
  geom_vline(xintercept = mean(church_hill$YrBuilt), color = "blue")
  
westover <- buildings_df %>%
  filter(neighborhood == "Westover")
  
ggplot(westover, aes(x = YrBuilt)) +
  geom_histogram() +
  geom_vline(xintercept = median(westover$YrBuilt), color = "red") +
  geom_vline(xintercept = mean(westover$YrBuilt), color = "blue")
  
westover_v_church_hill <- buildings_df %>%
  filter(neighborhood == "Westover" | neighborhood == "Church Hill")

ggplot(westover_v_church_hill, aes(y = YrBuilt, x = neighborhood,
                                   fill = neighborhood)) +
  geom_boxplot()
  
# Stacked Percentage Barplot unaggregated
buildings_df %>%
  dplyr::count(gent_n_eligible, ImprType) %>%
  group_by(gent_n_eligible) %>%
  mutate(countT= sum(n)) %>%
  group_by(ImprType, add=TRUE) %>%
  mutate(per=paste0(round(100*n/countT,2),'%')) %>%
  arrange(gent_n_eligible, desc(per)) %>%
  ggplot(aes(x = gent_n_eligible, y = n, fill = reorder(ImprType, desc(per)))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = per), position = position_fill(vjust = 0.5), size = 2) +
  theme_minimal()

# Stacked Percentage Barplot neighborhood level
neighborhood_sbc<- buildings_df %>%
  dplyr::count(neighborhood, gent_n_eligible, ImprType) %>%
  group_by(neighborhood, gent_n_eligible) %>%
  mutate(countT= sum(n)) %>%
  group_by(ImprType, add=TRUE) %>%
  mutate(per = 100 * n / countT) %>%
  group_by(gent_n_eligible, ImprType) %>%
  summarise(mean_per = mean(per, na.rm = TRUE),
            avg_per = mean(per, na.rm = TRUE)) %>%
  mutate(mean_per = paste0(round(mean_per, 2), "%")) %>%
  arrange(gent_n_eligible, desc(mean_per)) %>%
  ggplot(aes(x = gent_n_eligible, y = avg_per, fill = reorder(ImprType, 
                                                        desc(mean_per)))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = mean_per), position = position_fill(vjust = 0.5), size = 2) +
  theme_minimal()


#
neighborhood_pcdesc<- buildings_df %>%
  dplyr::count(neighborhood, gent_n_eligible, PCDesc) %>%
  group_by(neighborhood, gent_n_eligible) %>%
  mutate(countT= sum(n)) %>%
  group_by(PCDesc, add=TRUE) %>%
  mutate(per = 100 * n / countT) %>%
  group_by(gent_n_eligible, PCDesc) %>%
  summarise(mean_per = mean(per, na.rm = TRUE),
            avg_per = mean(per, na.rm = TRUE)) %>%
  mutate(mean_per = paste0(round(mean_per, 2), "%")) %>%
  arrange(gent_n_eligible, desc(mean_per)) %>%
  ggplot(aes(x = gent_n_eligible, y = avg_per, fill = reorder(PCDesc, 
                                                              desc(mean_per)))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = mean_per), position = position_fill(vjust = 0.5), size = 2) +
  theme_minimal()













