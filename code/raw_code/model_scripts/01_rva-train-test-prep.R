###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(rpostgis)
library(RPostgreSQL)
library(sf)
library(dplyr)
library(stringr)
library(tidyr)
source("../helper_scripts/helper00_project-db-connection.R")

dbGetQuery(spatialdb, "SELECT * 
                        FROM pg_catalog.pg_tables 
                          WHERE schemaname = 'public'") 


# Load
prop_nbh <- dbGetQuery(defaultdb, "SELECT * from properties_w_neighborhoods")

prop_nbh$PCDesc <- as.factor(prop_nbh$PCDesc)
prop_nbh$AssocNam <- as.factor(prop_nbh$AssocNam)
prop_nbh$ImprType <- as.factor(prop_nbh$ImprType)
prop_nbh$UseDesc <- as.factor(prop_nbh$UseDesc)
prop_nbh$BldgType <- as.factor(prop_nbh$BldgType)
prop_nbh$prime_const_type <- as.factor(prop_nbh$prime_const_type)
prop_nbh$CondDesc <- as.factor(prop_nbh$CondDesc)
prop_nbh$RoofDesc <- as.factor(prop_nbh$RoofDesc)


# Aggregate
agg_cont_df <- prop_nbh %>%
  select(neighborhood, gent_n_eligible, age_in_years, stories_fixed, FinSize, 
         NumRms, NumBdRms) %>%
  group_by(neighborhood, gent_n_eligible) %>%
  summarise(num_props = n(),
            med_age = median(age_in_years, na.rm = TRUE), 
            iqr_age = IQR(age_in_years, na.rm = TRUE),
            med_stories = median(stories_fixed, na.rm = TRUE),
            iqr_stories = IQR(stories_fixed, na.rm = TRUE),
            med_finsize = median(FinSize, na.rm = TRUE),
            iqr_finsize = IQR(FinSize, na.rm = TRUE),
            med_rooms = median(NumRms, na.rm = TRUE),
            iqr_rooms = IQR(NumRms, na.rm = TRUE),
            med_bdrooms = median(NumBdRms, na.rm = TRUE),
            iqr_bdrooms = IQR(NumBdRms, na.rm = TRUE))

agg_cat_df <- prop_nbh %>%
  select(neighborhood, gent_n_eligible, PCDesc, AssocNam, ImprType, UseDesc,
         BldgType, prime_const_type, CondDesc, RoofDesc) %>%
  mutate(
    num_r_one_st = case_when(trimws(PCDesc, which = "right") == "R One Story" 
                             ~ 1, TRUE ~ 0),
    num_r_two_st = case_when(trimws(PCDesc, which = "right") == "R Two Story" 
                             ~ 1, TRUE ~ 0),
    num_r_condo = case_when(trimws(PCDesc, which = "right") == 
                              "R Condo Residential 50+ Units" ~ 1, TRUE ~ 0),
    num_r_split = case_when(trimws(PCDesc, which = "right") == 
                              "R SplitLevel (Tri,Quad,Split)" ~ 1, TRUE ~ 0),
    num_w_assoc = case_when(trimws(AssocNam, which = "right") == "None" ~ 0,
                            TRUE ~ 1),
    num_comm = case_when(trimws(ImprType, which = "right") == "COMMERCIAL" ~ 1, 
                         TRUE ~ 0),
    num_dwell = case_when(trimws(ImprType, which = "right") == "DWELLING" ~ 1,
                          TRUE ~ 0),
    num_sing_fam = case_when(trimws(UseDesc, which = "right")
                             == "Single family" ~ 1, TRUE ~ 0),
    num_condo_hr = case_when(trimws(UseDesc, which = "right") == "Condo-HRise" 
                             ~ 1, TRUE ~ 0),
    num_dupl = case_when(trimws(UseDesc, which = "right") == "Duplex" ~ 1, 
                         TRUE ~ 0),
    num_gable = case_when(trimws(RoofDesc, which = "right") == "Gable" ~ 1, 
                          TRUE ~ 0),
    num_wfws = case_when(trimws(prime_const_type, which = "right") == 
                           "Wood Frame W/Sheathing" ~ 1, TRUE ~ 0),
    num_brick = case_when(trimws(prime_const_type, which = "right") == "Brick" 
                          ~ 1, TRUE ~ 0)
  ) %>%
  select(neighborhood, gent_n_eligible, num_r_one_st, num_r_two_st, num_r_condo,
         num_r_split, num_w_assoc, num_comm, num_dwell, num_sing_fam, 
         num_condo_hr, num_dupl, num_gable, num_wfws, num_brick) %>%
  group_by(neighborhood, gent_n_eligible) %>%
  summarise(pct_r_one_st = round((sum(num_r_one_st) / n()) * 100, digits = 2),
            pct_r_two_st = round((sum(num_r_two_st) / n()) * 100, digits = 2),
            pct_r_condo = round((sum(num_r_condo) / n()) * 100, digits = 2),
            pct_r_split = round((sum(num_r_split) / n()) * 100, digits = 2),
            pct_comm = round((sum(num_comm) / n()) * 100, digits = 2),
            pct_dwell = round((sum(num_dwell) / n()) * 100, digits = 2),
            pct_sing_fam = round((sum(num_sing_fam) / n()) * 100, digits = 2),
            pct_condo_hr = round((sum(num_condo_hr) / n()) * 100, digits = 2),
            pct_dupl = round((sum(num_dupl) / n()) * 100, digits = 2),
            pct_gable = round((sum(num_gable) / n()) * 100, digits = 2),
            pct_wfws = round((sum(num_wfws) / n()) * 100, digits = 2),
            pct_brick = round((sum(num_brick) / n()) * 100, digits = 2))

# Load
nbh_density <- dbGetQuery(defaultdb, "SELECT * from rva_neighborhood_density")
nbh_typology <- dbGetQuery(defaultdb, "SELECT * from rva_neighborhood_typology")

# Join sets
set_large <- nbh_density %>%
  select(Name, built_unbuilt_ratio) %>%
  inner_join(nbh_typology, by = "Name") %>%
  select(-gent_n_eligible) %>%
  right_join(agg_cont_df, by = c("Name" = "neighborhood")) %>%
  select(-gent_n_eligible) %>%
  right_join(agg_cat_df, by = c("Name" = "neighborhood")) %>%
  replace_na(list(built_unbuilt_ratio = 0, med_bdrooms = 3, iqr_bdrooms = 1)) %>%
  select(-Apartment_Court, -School_Campus, -Estate_Neighborhood, -Open_Space,
         -Suburban_Shopping_and_Business_Park, -Water, -pct_comm, -pct_dwell,
         -med_bdrooms, -iqr_bdrooms, -pct_r_condo, -pct_r_split) %>%
  replace(., is.na(.), 0)

set_large_names <- c("Name", "built_unbuilt_ratio", "Downtown", "Highways",
                     "Historic_Urban_Neighborhood", "Industrial_Land", 
                     "Post_Industrial_Zone", "Post_War_Suburb",
                     "Streetcar_Neighborhood", "num_props", "med_age", 
                     "iqr_age", "med_stories", "iqr_stories", "med_finsize",
                     "iqr_finsize", "med_rooms", "iqr_rooms", "gent_n_eligible",
                     "pct_r_one_st", "pct_r_two_st", "pct_sing_fam", 
                     "pct_condo_hr", "pct_dupl", "pct_gable", "pct_wfws",
                     "pct_brick")

names(set_large) <- set_large_names

set_large$gent_n_eligible <- as.factor(set_large$gent_n_eligible)

summary_of_set <- summary(set_large)

nefg_set <- set_large %>%
  filter(gent_n_eligible == "Not Eligible for Gentification")

edng_set <- set_large %>%
  filter(gent_n_eligible == "Eligible, Did Not Gentrify")

gent_set <- set_large %>%
  filter(gent_n_eligible == "Gentrified")

set_large2 <- set_large %>%
  select(-gent_n_eligible, -Name)

set_large3 <- set_large2 %>%
  select(pct_sing_fam, pct_gable, pct_wfws, num_props, med_age)

# EDA Plots
props_vs_ratio <- set_large %>%
  ggplot(aes(x = built_unbuilt_ratio, y = num_props, color = gent_n_eligible)) +
  geom_point()

dwnt_hwy <- set_large %>%
  ggplot(aes(x = Highways, y = Downtown, color = gent_n_eligible)) +
  geom_point() +
  labs(title = "Percent Downtown Vs Highway Coverage of Neighborhoods",
       x = "Percent Highway Coverage", y = "Percent Downtown Coverage",
       color = "Key")

ggsave(filename = "../../../figures/exploratory_figures/07_dwnt-hwy-scatterplot.png",
       dwnt_hwy)



# Save data for models to DB
dbWriteTable(defaultdb, "data_for_models", set_large, overwrite = TRUE, 
             row.names = FALSE)

# Set seed for reproduction
set.seed(182)

# Split into training, testing and validation datasets
train_set <- set_large %>%
  mutate(
    samp_weight = case_when(gent_n_eligible == "Eligible, Did Not Gentrify" ~ .20,
                            gent_n_eligible == "Not Eligible for Gentification" ~ .28,
                            gent_n_eligible == "Gentrified" ~ .52)
  ) %>%
  sample_frac(size = 0.6) %>%
  sample_frac(size = 1, weight = samp_weight, replace = TRUE)

test_set <- set_large %>%
  anti_join(train_set) %>%
  sample_frac(size = 0.5)

valid_set <- set_large %>%
  anti_join(train_set) %>%
  anti_join(test_set)

# Save training, testing, and validation data for models to DB
dbWriteTable(defaultdb, "training_data", train_set, overwrite = TRUE, 
             row.names = FALSE)

dbWriteTable(defaultdb, "testing_data", test_set, overwrite = TRUE, 
             row.names = FALSE)

dbWriteTable(defaultdb, "validation_data", valid_set, overwrite = TRUE, 
             row.names = FALSE)




######### knn ##########
library(class)

normalize <- function(x){
  (x -min(x))/(max(x)-min(x))   
  }

train_norm <- as.data.frame(lapply(train_set[,c(2:18, 20:27)], normalize))
test_norm <- as.data.frame(lapply(test_set[,c(2:18, 20:27)], normalize))

test_norm <- test_norm %>%
  replace(., is.na(.), 0) 

target_cat <- train_set$gent_n_eligible
test_cat <- test_set$gent_n_eligible

pr <- knn(train_norm, test_norm, cl = target_cat, k = 10)

# results
tab3 <- table(pr, test_cat)
accuracy(tab3)

# Validation
val_norm <- as.data.frame(lapply(valid_set[,c(2:18, 20:27)], normalize))

val_norm <- val_norm %>%
  replace(., is.na(.), 0) 

val_cat <- valid_set$gent_n_eligible
pr_val <- knn(train_norm, val_norm, cl = target_cat, k = 10)

tab3_val <- table(pr, val_cat)
accuracy(tab3_val)

#### plots
library(ggthemes)
iqr_age_bp <- set_large %>%
   mutate(gent_status = case_when(as.character(gent_n_eligible) == 
                                    "Not Eligible for Gentification" ~ 
                      "Not Eligible for Gentrification",
                    TRUE ~ as.character(gent_n_eligible))) %>%
   ggplot(aes(x = gent_status, y = iqr_age, fill = gent_status)) +
   geom_boxplot() +
   labs(title = "IQR of Properties Age per Neighborhood Boxplot", 
        x = "Gentrification Label", 
        y = "IQR of Properties Age per Neighborhood",
        fill = "Key") +
   theme_tufte()

pct_gable_bp <- set_large %>%
  mutate(gent_status = case_when(as.character(gent_n_eligible) == 
                                   "Not Eligible for Gentification" ~ 
                                   "Not Eligible for Gentrification",
                                 TRUE ~ as.character(gent_n_eligible))) %>%
  ggplot(aes(x = gent_status, y = pct_gable, fill = gent_status)) +
  geom_boxplot() +
  labs(title = "Percents of Properties with Gable Roofs per Neighborhood \nBoxplot", 
       x = "Gentrification Label", 
       y = "Percents of Properties per Neighborhood",
       fill = "Key") +
  theme_tufte()

downtown_bp <- set_large %>%
  mutate(gent_status = case_when(as.character(gent_n_eligible) == 
                                   "Not Eligible for Gentification" ~ 
                                   "Not Eligible for Gentrification",
                                 TRUE ~ as.character(gent_n_eligible))) %>%
  ggplot(aes(x = gent_status, y = Downtown, fill = gent_status)) +
  geom_boxplot() +
  labs(title = "Percents of Downtown Coverage per Neighborhood \nBoxplot", 
       x = "Gentrification Label", 
       y = "Percents of Coverage per Neighborhood",
       fill = "Key") +
  theme_tufte()

iqr_finsize_bp <- set_large %>%
  mutate(gent_status = case_when(as.character(gent_n_eligible) == 
                                   "Not Eligible for Gentification" ~ 
                                   "Not Eligible for Gentrification",
                                 TRUE ~ as.character(gent_n_eligible))) %>%
  ggplot(aes(x = gent_status, y = iqr_finsize, fill = gent_status)) +
  geom_boxplot() +
  labs(title = "IQR of Properties Finished Sizes per Neighborhood Boxplot", 
       x = "Gentrification Label", 
       y = "IQR of Properties Finished Seizes per Neighborhood",
       fill = "Key") +
  theme_tufte()

built_unbuilt_bp <- set_large %>%
  mutate(gent_status = case_when(as.character(gent_n_eligible) == 
                                   "Not Eligible for Gentification" ~ 
                                   "Not Eligible for Gentrification",
                                 TRUE ~ as.character(gent_n_eligible))) %>%
  ggplot(aes(x = gent_status, y = built_unbuilt_ratio, fill = gent_status)) +
  geom_boxplot() +
  labs(title = "Ratios of Built to Open Spaces per Neighborhood Boxplot", 
       x = "Gentrification Label", 
       y = "Ratios per Neighborhood",
       fill = "Key") +
  theme_tufte()





