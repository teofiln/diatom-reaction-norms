# summarise _all_ data into one frame to work with the flexdashboard
library(tidyverse)
library(lubridate)

# load experiment meta data
meta_path <- list.files(path = "./data", pattern = "metadata.csv", full.names = TRUE, recursive = TRUE)
all_plates_meta <- read_csv(meta_path)

# if your data is in a binary R file, comment the line above and uncomment + edit the path below:
# all_plates_meta <- get(load("metadata/metadata.Rsave"))

# get the file names
(
  files <-
    list.files(
      path = "./data",
      pattern = regex("*raw.txt|*raw.csv"),
      full.names = TRUE,
      recursive = TRUE
    )
)

# make names for all plates
plate_names <- basename(files)

# read the files and name the data frames with info from file names
all_plates <- map(files, read_csv)
names(all_plates) <- plate_names
cols_to_gather <- colnames(all_plates[[1]])[2:ncol(all_plates[[1]])]

# get summary stats
(
  all_plates_summarised <-
    bind_rows(all_plates, .id = "file_name") %>%
    group_by(file_name) %>%
    gather(well, Local_read, cols_to_gather) %>%
    select(file_name, well, Local_read) %>%
    group_by(file_name, well) %>%
    summarise_at(
      "Local_read",
      funs(
        "Mean" = mean(., na.rm=TRUE),
        "SD" = sd(., na.rm=TRUE),
        "Median" = median(., na.rm=TRUE),
        "Sum" = sum(., na.rm=TRUE)
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
#%>%
#    mutate(replicate = str_extract(plate_name, regex("\\d{1}")))
)

# join with meta data table
dd <-
  left_join(all_plates_meta,
            all_plates_summarised,
            by = c("plate_name", "well"))

write_csv(dd, path = paste("./data/measurements/", "all_data", ".csv", sep = ""))
save(dd, file = paste("./data/measurements/", "all_data", ".Rsave", sep = ""))
