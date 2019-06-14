# To prep the data when an experiment has vessels of different sizes
# and when measurements come from both a plate-based and a cuvete-based fluorometer
# the plate reader returns several reads per well, whereas the cuvete reader returns one
# so we need different logic to put the data together
# and the metadata sheet should include a treatment column that accounts for vessel type

# The metadata should contain information about vessel type like so, in a separate column:
# plate_name, sample,	strain,	treatment, row, column, well, replicate, vessel_type
# Where vessel_type is either 'plate' or 'flask'.
# But the distinction really is between files output from a SynergyH1 plate reader (Biotek) and Trilogy (Turner)
# Our plate protocol is scanning the bottom of each well along a grid, 
# so there are multiple reads for each well that need to be summarised prior to loading in the app.
# In turn, the Trilogy returns a single value per cuvete, that is 'mock' summarised 
# So, if the measuring protocol returns a single value, or if preprocessing to a single value is done, 
# use 'flask' in the metadata.

# Note also that in addition to the factor of primary interest, 
# the `treatment` column should reflect the vessel type as well.
# for example: 8ppt-F (for flasks) and 8ppt-P (for plates)

# !! IMPORTANT !! :
# We make the assumption that all plate reader files will have `csv` or `txt` file extension,
# and that all Trilogy files will have a `tsv` extension reflecting the delimiter. 
# Alter the script accordignly if these conditions don't match your design.

library(tidyverse)
library(lubridate)

path_to_experiment <- "./data"

meta_path <- list.files(path = path_to_experiment, pattern = "metadata-plates-and-tubes.csv", full.names = TRUE, recursive = TRUE)
all_meta <- read_csv(meta_path)

##### --- Platereader -------------------- #####
# get the file names
(
  files <-
    list.files(
      path = path_to_experiment,
      pattern = regex("*raw.txt|*raw.csv"),
      full.names = TRUE,
      recursive = TRUE
    )
)

if (length(files) != 0) { 
  message("Did not find any plate reader files.")
# make names for all plates
plate_names <- basename(files)

# read the files and name the data frames with info from file names
all_plates <- map(files, read_csv)
names(all_plates) <- plate_names

# get summary stats
(
  all_plates_summarised <-
    bind_rows(all_plates, .id = "file_name") %>%
    group_by(file_name) %>%
    gather(well, Local_read, -file_name, -`X Read #`, -`Y Read #`) %>%
    select(file_name, well, Local_read) %>%
    group_by(file_name, well) %>%
    summarise_at(
      "Local_read",
      list(
        "Mean" = ~mean(., na.rm=TRUE),
        "SD" = ~sd(., na.rm=TRUE),
        "Median" = ~median(., na.rm=TRUE),
        "Sum" = ~sum(., na.rm=TRUE)
      )
    ) %>% 
      # parse the info from file name to get plate name, date and transfer
    separate(
      file_name,
      into = c("plate_name", "transfer", "year", "month", "day"),
      sep = "-",
      extra = "drop"
    ) %>% 
      ungroup %>%
    mutate(date = ymd(paste(
      year, month, day, sep = "-"
    ))) %>%
    select(-year,-month,-day) %>%
    mutate(day = as.numeric(date - min(date))) 
)

# join with meta data table

all_plates_meta <- all_meta %>% filter(vessel_type=="plate") 

dd <-
  inner_join(all_plates_meta,
            all_plates_summarised,
            by = c("plate_name", "well"))
}

##### --- Trilogy -------------------- #####

t_files <- list.files(path_to_experiment, pattern = "*.tsv", full.names = TRUE, recursive = TRUE)
t_plate_names <- basename(t_files)
t_all_plates <- map(t_files, read_tsv, col_names=c("sample", "RFU"), col_types="cn__")
names(t_all_plates) <- t_plate_names

if (length(t_files) != 0) {
  message("Did not find any Trilogy files.")
  
(
  all_tubes_summarised <-
    bind_rows(t_all_plates, .id = "file_name") %>%
    # add 'summary stats'
    mutate(
      "Mean" = RFU,
      "SD" = NA,
      "Median" = RFU,
      "Sum" = RFU
    ) %>% 
    # extract the sample number to be used for join later
    mutate(number=str_extract(sample, regex("\\d+")) %>% str_remove(., regex("^0+")) %>% as.numeric()) %>% 
    mutate(well=paste("A", number, sep="")) %>% 
    select(-sample, -RFU) %>% 
    mutate(fn=str_remove(file_name, ".tsv")) %>% 
    separate(fn, into=c("experiment", "transfer", "year", "month", "day"), sep = "-") %>% 
    mutate(date=ymd(paste(year, month, day, sep="-"))) %>% 
    select(-year, -month, -day, -experiment, -file_name) %>% 
    mutate(day=as.numeric(date- min(date)))
)

all_tubes_meta <- all_meta %>% filter(vessel_type=="flask")

# join with meta data table
t_dd <- inner_join(all_tubes_meta, all_tubes_summarised, by = "well")

}

# concatenate both data sets
if (length(files) != 0 & length(t_files) != 0) {
  all_dd <- bind_rows(dd, t_dd)  
}

if (length(files) == 0 & length(t_files) != 0) {
  all_dd <- t_dd
}

if (length(files) != 0 & length(t_files) == 0) {
  all_dd <- dd
}

# save
write_csv(all_dd, path = paste("./data/measurements/", "all_data", ".csv", sep = ""))
save(all_dd, file = paste("./data/measurements/", "all_data", ".Rsave", sep = ""))
