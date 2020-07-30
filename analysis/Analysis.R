library(dplyr)
library(tidyr)
library(ggplot2)
library(readODS)

reported_emissions <- read_ods("../data/reported_emissions.ods")
reported_by_year <- reported_emissions %>% 
  mutate(number = as.factor(number)) %>% 
  group_by(name, year) %>% 
  summarise(value_reported = mean(value, na.rm = TRUE))

remote_emissions <- read_ods("../data/catalogue_ru_2019.ods", skip = 2)
differences <- remote_emissions %>% 
  select(name = NAME, y2005:y2019) %>% 
  gather(year, value_remote, y2005:y2019) %>% 
  mutate(year = as.numeric(sub("y", "", year, fixed = TRUE)),
         value_remote = value_remote * 1000) %>% 
  as_tibble() %>% 
  inner_join(reported_by_year, by = c("name", "year")) %>% 
  mutate(diff = (value_reported - value_remote) / value_remote) %>% 
  drop_na(diff)

differences %>% 
  filter(diff < 10) %>% 
  ggplot(aes(x = diff)) +
  geom_density()
  

