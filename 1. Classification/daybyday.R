library(tidyverse)
library(dplyr)              # Load the dplyr package
library(ggplot2)            # Load the ggplot2 package
library(tibble)
library(openxlsx)
library(dplyr)
library(ggplot2)
# Load the data
cases_TX <- read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/COVID-19_cases_TX.csv")
cases_Dallas <- cases_TX %>% filter(county_name == "Dallas County" & state == "TX")

# Calculate the difference from the previous day
cases_Dallas <- cases_Dallas %>%
  arrange(date) %>%
  mutate(confirmed_cases_diff = confirmed_cases - lag(confirmed_cases, default = 0))

# Extract the value for January 19, 2021
value_2021_01_19 <- cases_Dallas %>% filter(date == "2021-01-19") %>% pull(confirmed_cases_diff)

# Create the plot
ggplot(cases_Dallas, aes(x = date, y = confirmed_cases_diff)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2021-01-19"), linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2021-01-19"), y = max(cases_Dallas$confirmed_cases_diff), 
           label = paste("Value: ", value_2021_01_19),
           vjust = 3, hjust = 2, color = "red") +
  geom_point(data = cases_Dallas %>% filter(date == "2021-01-19"), 
             aes(x = date, y = confirmed_cases_diff), color = "red", size = 3) +
  labs(title = "Dallas County Daily Number of Cases")
#######################

cases_Harris <- cases_TX %>% filter(county_name == "Harris County" & state == "TX")

# Calculate the difference from the previous day
cases_Harris <- cases_Harris %>%
  arrange(date) %>%
  mutate(confirmed_cases_diff = confirmed_cases - lag(confirmed_cases, default = 0))

# Extract the value for January 19, 2021
value_2021_01_19 <- cases_Harris %>% filter(date == "2021-01-19") %>% pull(confirmed_cases_diff)

# Create the plot
ggplot(cases_Harris, aes(x = date, y = confirmed_cases_diff)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2021-01-19"), linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2021-01-19"), y = max(cases_Harris$confirmed_cases_diff), 
           label = paste("Value: ", value_2021_01_19),
           vjust = 3, hjust = 2, color = "red") +
  geom_point(data = cases_Harris %>% filter(date == "2021-01-19"), 
             aes(x = date, y = confirmed_cases_diff), color = "red", size = 3) +
  labs(title = "Harris County Daily Number of Cases")
