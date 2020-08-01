library(dplyr)
library(tidyr)
library(ggplot2)
library(readODS)

summarise_reported <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  if (length(x) == 2)
    x <- ifelse(min(x) / max(x) > 0.1, mean(x), max(x))
  if (length(x) > 2)
    x <- ifelse(min(x) / max(x) > 0.1, mean(x), mean(x[-which.min(x)]))
  return(round(x))
}

reported_emissions <- read_ods("../data/reported_emissions.ods")
reported_by_year <- reported_emissions %>% 
  mutate(name = as.factor(name)) %>% 
  group_by(name, year) %>% 
  summarise(value_reported = summarise_reported(value)) %>% 
  drop_na(value_reported)

reported_nrows_filled <- reported_by_year %>% group_by(name) %>% summarise(nrows_filed = n())

shapiro.test(reported_by_year$value_reported)
qplot(reported_by_year$value_reported, geom = "histogram", binwidth = 10000)

reported_by_year_without_Norilsk <- reported_by_year[reported_by_year$name != "Norilsk", ]
shapiro.test(reported_by_year_without_Norilsk$value_reported)
qplot(reported_by_year_without_Norilsk$value_reported, geom = "histogram", binwidth = 10000)


remote_emissions <- read_ods("../data/catalogue_ru_2019.ods", skip = 2)
remote_by_year <- remote_emissions %>% 
  select(name = NAME, y2005:y2019) %>% 
  gather(year, value_remote, y2005:y2019) %>% 
  mutate(name = as.factor(name),
    year = as.numeric(sub("y", "", year, fixed = TRUE)),
    value_remote = round(value_remote * 1000)) %>% 
  as_tibble() %>% 
  drop_na(value_remote)

shapiro.test(remote_by_year$value_remote)
qplot(remote_by_year$value_remote, geom = "histogram", binwidth = 1e+04)

remote_by_year_without_Norilsk <- remote_by_year[remote_by_year$name != "Norilsk", ]
shapiro.test(remote_by_year_without_Norilsk$value_remote)
qplot(remote_by_year_without_Norilsk$value_remote, geom = "histogram", binwidth = 1e+04)


data <- remote_by_year %>% 
  inner_join(reported_by_year, by = c("name", "year"))

classify_difference <- function (x) {
  amount = x[1]
  ratio = x[2]
  threshold <- 0.61
  if (amount > 1e+5) threshold <- 0.55
  if (amount < 0.5e+4) threshold <- 0.67
  
  if (ratio < 1 - threshold) return(-1) # Reported is significantly lower than remote
  if (ratio > 1 + threshold) return(1) # Reported is significantly higher than remote
  return(0) # The difference between remote and reported is within the uncertainty level
}

data <- data %>% 
  mutate(ratio = value_reported / value_remote)

data$diff_category <- apply(select(data, value_remote, ratio), 1, classify_difference)

diff_cats_table <- table(data$diff_category)

diff_cats_table_by_source <- data %>% 
  group_by(name, diff_category) %>% 
  summarise(n = n())

data %>% 
  dplyr::filter(value_reported > 30000) %>% 
  group_by(diff_category) %>% 
  summarise(n = n())

median(data$value_remote)
median(data$value_reported)
median(data$value_remote - data$value_reported)
wilcox.test(data$value_remote, data$value_reported, paired = TRUE, conf.int = TRUE)

ggplot() +
  geom_histogram(aes(x = data$value_remote), binwidth = 10000, fill = "red", alpha = .5) +
  geom_histogram(aes(x = data$value_reported), binwidth = 10000, fill = "green", alpha = .5)

differences %>% 
  dplyr::filter(diff < 10) %>% 
  ggplot(aes(x = diff)) +
  geom_density()
  
min(differences$diff)

qplot(differences[differences$value_remote < 1000000, "value_remote"] %>% pull(), geom = "density")

x <- rchisq(10000, 7) * 10000 + 30000
qplot(x, geom = "density")

sim_data <- data.frame(remote = x)
sim_data$diff = rnorm(10000, sd = 0.5) * sim_data$remote
sim_data$reported = sim_data$remote - sim_data$diff
sim_data$reported[sim_data$reported < 0] <- 0
sim_data$index = (sim_data$remote - sim_data$reported) / (sim_data$remote + sim_data$reported)
qplot(sim_data$index, geom = "density")


wilcox.test(differences$value_remote, differences$value_reported, paired = TRUE, conf.int = TRUE)
median(differences$value_remote)
median(differences$value_reported)


differences <- differences %>% 
  mutate(diff_cat = if_else((value_remote - value_reported) / value_remote < -0.58, -1,
                            if_else((value_remote - value_reported) / value_remote > 0.58, 1, 0)))
table(differences$diff_cat)
