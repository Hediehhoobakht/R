library(tidyverse)

# Load the data
cases_TX <- read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/COVID-19_cases_TX.csv")
cases_Dallas <- cases_TX %>% filter(county_name == "Dallas County" & state == "TX")

# Calculate the difference from the previous day
cases_Dallas <- cases_Dallas %>%
  arrange(date) %>%
  mutate(confirmed_cases_diff = confirmed_cases - lag(confirmed_cases, default = 0))

# Extract the value for January 19, 2021
value_2021_01_19 <- cases_Dallas %>% filter(date == "2021-01-19") %>% pull(confirmed_cases_diff)

# Define the US holidays within your date range
holidays <- data.frame(
  holiday_date = as.Date(c("2020-12-25", "2021-01-01", "2020-07-04", "2020-11-26", "2020-10-31")),
  holiday_name = c("Christmas", "New Year's", "4th of July", "Thanksgiving", "Halloween")
)

# Calculate the value for each holiday
holidays <- holidays %>%
  left_join(cases_Dallas, by = c("holiday_date" = "date")) %>%
  mutate(holiday_name = ifelse(is.na(holiday_name), "", holiday_name))

# Create a separate data frame for holiday points
holiday_points <- data.frame(
  date = holidays$holiday_date,
  value = holidays$confirmed_cases_diff,
  label = paste("Value: ", holidays$confirmed_cases_diff, "\n", holidays$holiday_name)
)

# Create the plot
p <- ggplot(cases_Dallas, aes(x = date, y = confirmed_cases_diff)) +
  geom_line() +
  geom_smooth()+
  geom_point(data = cases_Dallas %>% filter(date == "2021-01-19"), 
             aes(x = date, y = confirmed_cases_diff), color = "red", size = 2) +
  geom_point(data = holiday_points, aes(x = date, y = value), color = "cyan", size = 2) +
  labs(title = "Dallas County Daily Number of Cases")


p
