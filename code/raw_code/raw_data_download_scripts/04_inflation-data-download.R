# CPI - Consumer Price Index

url <-"https://download.bls.gov/pub/time.series/cu/cu.data.0.Current"

dest_file <- "../../../data/raw_data/cpi.txt"

download.file(url = url, destfile = dest_file)

#CUSR0000SA0

cpis <- read.table(dest_file, header = TRUE, sep = "\t", 
                   stringsAsFactors = FALSE)

cpis_2000_2017 <- cpis %>%
  mutate(series_id = trimws(series_id, which = "right")) %>%
  filter(series_id == "CUSR0000SA0" & year >= 2000 & year <= 2017) %>%
  select(year, value) %>%
  group_by(year) %>%
  summarise(value = mean(value))