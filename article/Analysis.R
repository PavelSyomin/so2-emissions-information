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

nrow_reported <- nrow(reported_by_year)

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
  mutate(ratio = value_reported / value_remote,
         diff = value_reported - value_remote)

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

table_1 <- data %>% 
  mutate(amount_group = cut(value_remote, c(0, 50000, 100000, 1e+7), labels = c("lt50", "50to100", "gt100"), include.lowest = TRUE)) %>% 
  group_by(diff_category, amount_group) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(diff_category = factor(diff_category, labels = c("lt_remote", "eq_remote", "gt_remote"))) %>% 
  spread(diff_category, n) %>% 
  mutate(amount_group = factor(amount_group, labels = c("До 50 тысяч тонн", "От 50 до 100 тысяч тонн", "Больше 100 тысяч тонн"))) %>% 
  replace_na(replace = list(gt_remote = 0))

table_2 <- data %>% 
  mutate(amount_group = cut(value_remote, c(0, 50000, 100000, 1e+7), labels = c("lt50", "50to100", "gt100"), include.lowest = TRUE),
         reported_is_lower = if_else(diff < 0, 1, 0),
         amount_group = factor(amount_group, labels = c("До 50 тысяч тонн", "От 50 до 100 тысяч тонн", "Больше 100 тысяч тонн"))) %>% 
  group_by(amount_group, reported_is_lower) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  spread(reported_is_lower, n) %>% 
  select(amount_group, reported_is_lower = `1`, reported_is_higher = `0`)
  
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

median_values_without_Norilsk <- data %>% 
  filter(name != "Norilsk") %>% 
  summarise(remote = median(value_remote),
            reported = median(value_reported))

plot_1 <- data %>% 
  filter(name != "Norilsk") %>% 
  gather(data_source, amount, value_reported, value_remote) %>% 
  ggplot(aes(x = amount, linetype = data_source)) +
  geom_freqpoly(binwidth = 10000, boundary = 0, position = position_dodge2(padding = .5, width = 2)) +
  geom_vline(xintercept = median_values_without_Norilsk$remote, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = median_values_without_Norilsk$reported, linetype = "solid", color = "gray50") +
  scale_linetype_manual(name = "Источник данных", values = c("dashed", "solid"), labels = c("Дистанционные измерения", "Официальная отчётность")) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_y_continuous(expand = expand_scale(add = c(0, 3))) +
  annotate(geom = "text", label = paste0("M[отчёт] == ", format(median_values_without_Norilsk$reported, digits = 0, scientific = FALSE)), x = median_values_without_Norilsk$reported, y = 45, angle = 90, hjust = -0.1, vjust = 1.3, parse = TRUE, family = "PT Sans") +
  annotate(geom = "text", label = paste0("M[дист.] == ", format(median_values_without_Norilsk$remote, digits = 0, scientific = FALSE)), x = median_values_without_Norilsk$remote, y = 45, angle = 90, hjust = -0.1, vjust = 1.3, parse = TRUE, family = "PT Sans") +
  labs(x = "Объём выбросов, тысяч тонн в год", y = "Число источников загрязнения") +
  theme_bw(base_family = "PT Sans", base_size = 14) +
  theme(legend.position = c(.8, .8), panel.grid.minor.y = element_line(size = 0))
plot_1

data %>% 
  filter(value_reported > 85000, value_reported < 95000)

data %>% 
  filter(ratio != Inf, ratio < 3) %>% 
  ggplot(aes(x = ratio)) +
  geom_histogram(binwidth = 0.1, color = "red")

data %>% 
  filter(value_remote > 0.5e+5) %>% 
  group_by(year, diff_category) %>% 
  summarise(n = n()) %>% 
  spread(diff_category, n)

data %>% 
  filter(name == "Norilsk") %>% 
  mutate(reported_is_lower = if_else(value_reported - value_remote < 0, 1, 0)) %>% 
  group_by(reported_is_lower) %>% 
  summarise(n = n())

data %>% 
  filter(name == "Norilsk") %>% 
  summarise(max_ratio = max(ratio),
            min_ratio = min(ratio))

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


# A nice plot of remote and reported emissions in Norilsk, just for fun
data %>% 
  filter(name == "Norilsk") %>% 
  gather(source, amount, value_reported:value_remote) %>% 
  ggplot(aes(x = year, y = amount / 1e+6, color = source)) +
  geom_line() +
  geom_point(shape = 21, size = 2, fill = "gray10", stroke = 1.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1, linetype = "dotted", show.legend = FALSE) +
  scale_x_continuous(name = "", breaks = seq(2006, 2020, 2)) +
  scale_y_continuous(name = "", labels = function(x) format(x, decimal.mark = ",")) +
  scale_color_manual(name = "", labels = c("Спутник", "Отчётность"), values = c("#B88900", "gray80")) +
  labs(title = "Выбросы диоксида серы в Норильске", subtitle = "В миллионах тонн за 2005–2019 годы по спутниковым данным и официальной отчётности", caption = "Источники: https://so2.gsfc.nasa.gov/measures.html, https://www.nornickel.ru/sustainability/reporting/,\nhttp://www.mpr.krskstate.ru/envir/page5849") +
  theme_minimal(base_size = 16, base_family = "PT Sans") +
  theme(plot.background = element_rect(fill ="gray10"),
        text = element_text(color = "white"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray30", size = 0.25, linetype = "solid"),
        axis.text = element_text(color = "gray60"),
        legend.justification = c(0, 1),
        legend.position = c(0.1, 1),
        plot.subtitle = element_text(size = rel(1)),
        aspect.ratio = 9/16)
