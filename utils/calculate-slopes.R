library(tidyverse)
library(broom)

data_path <- list.files(path = "./data", pattern = "all_data.csv", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
dd <- read_csv(data_path)

slopes <- dd %>% 
  group_by(sample, strain, treatment, replicate, transfer) %>% 
  mutate(day_per_transfer=row_number()) %>% 
  filter(day_per_transfer %in% 2:6) %>% 
  nest() %>% 
  mutate(
    fit = map(data, ~ lm(log(Sum) ~ day_per_transfer, data = .)), results = map(fit, tidy)) %>% 
  unnest(results) %>% 
  filter(term=="day_per_transfer")

save(slopes, file=paste(dirname(data_path), "/slopes.Rsave", sep=""))
write_csv(slopes, path=paste(dirname(data_path), "/slopes.csv", sep=""))