library(tidyverse)
library(lubridate)

all_tubes_meta <- read_csv("metadata/tubes_meta.csv")

path_to_experiment <- "measurements"

files <- list.files(path_to_experiment, pattern = "*.tsv", full.names = TRUE, recursive = TRUE)
plate_names <- basename(files)
all_plates <- map(files, read_tsv, col_names=c("sample", "RFU"), col_types="cn__")
names(all_plates) <- plate_names

# get summary stats 
# !not really! 
# there are no summary stats, its a single measurement
# which is stored in `Sum`
# but make the columns to keep consistent for the app
# those contain NA

(
  all_tubes_summarised <-
    bind_rows(all_plates, .id = "file_name") %>%
    # add 'summary stats'
    mutate(
      "Mean" = RFU,
      "SD" = NA,
      "Median" = RFU,
      "Sum" = RFU
      ) %>% 
    # extract the sample number to be used for join later
    mutate(number=str_extract(sample, regex("\\d+")) %>% str_remove(., regex("^0+")) %>% as.numeric()) %>% 
    select(-sample, -RFU) %>% 
    mutate(fn=str_remove(file_name, ".tsv")) %>% 
    separate(fn, into=c("experiment", "transfer", "year", "month", "day"), sep = "-") %>% 
    mutate(date=ymd(paste(year, month, day, sep="-"))) %>% 
    select(-year, -month, -day, -experiment, -file_name) %>% 
    mutate(day=as.numeric(date- min(date)))
)

# join with meta data table
dd <- left_join(all_tubes_meta, all_tubes_summarised, by="number")
dd <- dd %>% mutate(well=paste(number, 1, sep="."))
# save to file
write_csv(dd, path = "measurements/all-data.csv")
