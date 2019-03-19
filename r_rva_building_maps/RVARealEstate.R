###############################################################################
## Author: Andrew Rosa                                                       ##
##                                                                           ##
## Notes:                                                                    ##
##                                                                           ##
##                                                                           ##
##                                                                           ##
###############################################################################


library(dplyr)
library(ggplot2)
library(ggmap)


master_df <- read.table("~/projects/rva/RealMaster.txt",
                        header = TRUE, sep = ",")
land_df <- read.table("~/projects/rva/RealLand.txt",
                        header = TRUE, sep = ",")
improve_df <- read.table("~/projects/rva/RealImprov.txt",
                         header = TRUE, sep = ",")




dist_improve_pins <- improve_df %>%
  distinct(PIN)

dist_master_pins <- master_df %>%
  distinct(PIN)
  
missing_props <- dist_master_pins %>%
  anti_join(dist_improve_pins) %>%
  arrange(PIN)

improv_sub_df <- improve_df %>%
  select(PIN, WCovDes1, HeatDesc, RoofDesc, RoofMatD, CondDesc, ExtDesc2,
         ExtDesc1,ConstFr, Foundat, BldgType, Stories, YrBuilt, FinSize, 
         UseDesc) %>%
  group_by(PIN) %>%
  arrange(YrBuilt, BldgType) %>%
  filter(YrBuilt == min(YrBuilt), row_number() == 1)

improv_count <- improv_sub_df %>%
  select(PIN) %>%
  count(PIN) %>%
  filter(n > 1)

master_sub_df <- master_df %>%
  select(PIN, LocAddr, LocCity, LocState, LocZip, PCDesc, AssocNam, NeiDesc)

buildings_df <- master_sub_df %>%
  inner_join(improv_sub_df, by = "PIN")

year_freq <- buildings_df %>%
  group_by(YrBuilt) %>%
  count(YrBuilt) %>%
  arrange(desc(n))


year_freq_plot <- ggplot(year_freq, aes(x = YrBuilt, y = n)) +
  geom_bar(stat="identity")

year_ts_plot <- ggplot(year_freq, aes(x = YrBuilt, y = n)) +
  geom_line()




