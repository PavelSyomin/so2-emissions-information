# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readODS)

# Define a function to summarise data on reported emissions.
# Sometimes there are more than one data source for each combination of pollution source and year, so we need to account for this.
summarise_reported <- function(x) {
  # Remove NAs (the amount of emissions is not present in the data source)
  x <- x[!is.na(x)]
  # If there is no data sources with amount of emissions, return NA
  if (length(x) == 0) return(NA)
  # If there is only one data source, return its value
  # If there are two or more data sources, we need to account for the fact that in some rare cases there are strange extremely low values in one of the data sources (e. g. 20000 in a state report, but 0 or 1 in Rosstat data sheet)
  # So if the minimal value of data sources is extremely small (e. g. less than 1/10 of the maximum value), we should exclude it and then calculate a mean.
  if (length(x) == 2)
    x <- ifelse(min(x) / max(x) > 0.1, mean(x), max(x))
  if (length(x) > 2)
    x <- ifelse(min(x) / max(x) > 0.1, mean(x), mean(x[-which.min(x)]))
  # Return a summarised value
  return(round(x))
}

# Read data on reported emissisons
reported_emissions <- read_ods("../data/reported_emissions.ods")
# Aggregate it so that one year for one pollution source is represented by only one value
reported_by_year <- reported_emissions %>% 
  mutate(name = as.factor(name)) %>% 
  group_by(name, year) %>% 
  summarise(value_reported = summarise_reported(value)) %>% 
  drop_na(value_reported)

# See for how many years the amount of pollution is known by each pollution source. The maximum is 15.
reported_nrows_filled <- reported_by_year %>% group_by(name) %>% summarise(nrows_filed = n())

# Check the distribution of reported values
shapiro.test(reported_by_year$value_reported)
qplot(reported_by_year$value_reported, geom = "histogram", binwidth = 10000)

# Exclude data for Norilsk because the amount of emissions there is extremely high
reported_by_year_without_Norilsk <- reported_by_year[reported_by_year$name != "Norilsk", ]
# Do the same checks for distribution (a hint: no difference)
shapiro.test(reported_by_year_without_Norilsk$value_reported)
qplot(reported_by_year_without_Norilsk$value_reported, geom = "histogram", binwidth = 10000)

# Load data on emissions measured by satellite
remote_emissions <- read_ods("../data/catalogue_ru_2019.ods", skip = 2)
# Turn the data frame to a long format to match the structure of reported_emissions
remote_by_year <- remote_emissions %>% 
  select(name = NAME, y2005:y2019) %>% 
  gather(year, value_remote, y2005:y2019) %>% 
  mutate(name = as.factor(name),
    year = as.numeric(sub("y", "", year, fixed = TRUE)),
    value_remote = round(value_remote * 1000)) %>% 
  as_tibble() %>% 
  drop_na(value_remote)

# Check the normality of remotely measured values
shapiro.test(remote_by_year$value_remote)
qplot(remote_by_year$value_remote, geom = "histogram", binwidth = 1e+04)

# Exclude Norilsk and do the same checks once again
remote_by_year_without_Norilsk <- remote_by_year[remote_by_year$name != "Norilsk", ]
shapiro.test(remote_by_year_without_Norilsk$value_remote)
qplot(remote_by_year_without_Norilsk$value_remote, geom = "histogram", binwidth = 1e+04)


# Join remote and reported data
data <- remote_by_year %>% 
  inner_join(reported_by_year, by = c("name", "year"))

# Function to check if the difference between remote and reported emissions exceeds the normal uncertainty level
# The normal uncertainty depends on the amount of emissions
# If the amount is bigger than 100 Kt per year, the uncertainty is 55%.
# If the amount is smaller that 50 Kt per year, the uncertainty is more than 67%.
# As a logical consequence, if the amount is between 50 and 100 Kt, the uncertainty is 67%.
# (Yes, we can't say for sure what the uncertainty for 50–100 Kt per year is, but it's the most straightforward assumption)
# Also we can't set uncertainty for less than 50 Kt to “more than 67%”, because we heed a constant value. Let it bee 0.79.
# See Fioletov et al., 2016. A global catalogue of large SO 2 sources and emissions derived from the Ozone Monitoring Instrument. P. 11503.
# Function is to be used in apply()
# Receives x, a vector of two, where x[1] is amount of emissions measured from a satellite, and x[2] is reported/remote ratio
classify_difference <- function(x) {
  # Assign amount and ratio variables
  amount = x[1]
  ratio = x[2]
  # Adjust a threshold depending on the amount of emissions
  threshold <- 0.67
  if (amount > 1e+5) threshold <- 0.55
  
  if (ratio < 1 - threshold) return(-1) # Reported is significantly lower than remote
  if (ratio > 1 + threshold) return(1) # Reported is significantly higher than remote
  return(0) # The difference between remote and reported is within the uncertainty level
}

# Calculate reported to remote emissions ratio
data <- data %>% 
  mutate(ratio = value_reported / value_remote)

# Assign one of three categories of difference: reported are significantly lower than remote (-1), reported are significantly higher than remote (1), reported are approximately equal to remote, i. e. the difference is within the uncertainty (0)
data$diff_category <- apply(select(data, value_remote, ratio), 1, classify_difference)

# Count the overall number of difference categories
diff_cats_table <- table(data$diff_category)

# Count the number of difference categories by pollution source
diff_cats_table_by_source <- data %>% 
  group_by(name, diff_category) %>% 
  summarise(n = n())

# Count the number of difference categories for pollution sources with emissions amount bigger than 100 Kt per year
# This group of pollution sources is the most important, because for such high pollution the uncertainty of remote meausures is the lowest
data %>% 
  dplyr::filter(value_remote > 100000) %>% 
  group_by(diff_category) %>% 
  summarise(n = n())

# Count the number of difference categories for pollution sources with emissions amount between 50 and 100 Kt per year
data %>% 
  dplyr::filter(value_remote > 50000, value_remote < 100000) %>% 
  group_by(diff_category) %>% 
  summarise(n = n())

# Count the number of difference categories for pollution sources with emissions amount lower than 50 Kt per year
data %>% 
  dplyr::filter(value_remote < 50000) %>% 
  group_by(diff_category) %>% 
  summarise(n = n())

# Compare reported and remote emissions as two paired samples
# First, calculate medians
median(data$value_remote)
median(data$value_reported)
median(data$value_remote - data$value_reported)
# Next, perform a Wilcoxon test (not t-test, because the distribution is not normal)
wilcox.test(data$value_remote, data$value_reported, paired = TRUE, conf.int = TRUE)

# Do the same for large pollution sources (more than 100 Kt per year) only
# Do the same for data without Norilsk
data %>% 
  dplyr::filter(value_remote > 1e+5) %>% 
  gather(data_source, value, value_remote:value_reported) %>% 
  group_by(data_source) %>% summarise(median = median(value))

data %>% 
  dplyr::filter(value_remote > 1e+5) %>% 
  gather(data_source, value, value_remote:value_reported) %>% 
  wilcox.test(value ~ data_source, .,  paired = TRUE, conf.int = TRUE)


data %>% 
  filter(name != "Norilsk") %>% 
  gather(data_source, amount, value_reported, value_remote) %>% 
  ggplot(aes(x = amount, fill = data_source)) +
  geom_histogram(binwidth = 1e+4, position = position_dodge())

data %>% 
  filter(ratio != Inf, ratio < 3) %>% 
  ggplot(aes(x = ratio)) +
  geom_histogram(binwidth = 0.1, color = "red")

data %>% 
  filter(value_remote > 0.5e+5) %>% 
  group_by(year, diff_category) %>% 
  summarise(n = n()) %>% 
  spread(diff_category, n)

sim_data <- data.frame(reported = data$value_reported)
sim_data$diff = rnorm(251, sd = 0.5) * sim_data$reported
sim_data$remote = -50000 + sim_data$reported - sim_data$diff
sim_data$remote[sim_data$remote < 0] <- 0
sim_data$index = (sim_data$remote - sim_data$reported) / (sim_data$remote + sim_data$reported)
sim_data$ratio <- sim_data$reported / sim_data$remote
qplot(sim_data$index, geom = "histogram")
sim_data %>% 
  filter(ratio < 3, ratio > -3) %>% 
  ggplot(aes(x = ratio)) + 
  geom_histogram(binwidth = 0.1)

nrow(sim_data[sim_data$ratio == Inf, ])
shapiro.test(sim_data$ratio)

wilcox.test(differences$value_remote, differences$value_reported, paired = TRUE, conf.int = TRUE)
median(differences$value_remote)
median(differences$value_reported)


differences <- differences %>% 
  mutate(diff_cat = if_else((value_remote - value_reported) / value_remote < -0.58, -1,
                            if_else((value_remote - value_reported) / value_remote > 0.58, 1, 0)))
table(differences$diff_cat)


qplot(data$ratio, geom = "density")

data %>%
  filter(value_remote < 50000) %>% 
  mutate(remote_is_lower = if_else(value_remote < value_reported, 1, 0)) %>% 
  group_by(remote_is_lower) %>% 
  summarise(n = n())
