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

# Filter the data for January 19, 2021
cases_Dallas_2021_01_19 <- cases_Dallas %>% filter(date == "2021-01-19")

# Define the US holidays within your date range
holidays <- data.frame(
  holiday_date = as.Date(c("2020-12-25", "2021-01-01", "2020-07-04", "2020-11-26", "2020-10-31")),
  holiday_name = c("Christmas", "New Year's", "4th of July", "Thanksgiving", "Halloween")
)

# Create the plot for COVID-19 cases
p_cases <- ggplot(cases_Dallas, aes(x = date, y = confirmed_cases)) +
  geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-19")), linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2021-01-19"), y = max(cases_Dallas$confirmed_cases), 
           label = cases_Dallas_2021_01_19$confirmed_cases, vjust = 2, hjust = 1.2) +
  geom_point(data = cases_Dallas_2021_01_19, aes(x = date, y = confirmed_cases), color = "red", size = 3)

# Create the plot for mobility data
mobility <- read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/Global_Mobility_Report.csv")
mobility <- read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/Global_Mobility_Report.csv", col_types =  cols(sub_region_2 = col_character()))
mobility <- mobility %>% mutate_if(is.character, factor)
mobility_Dallas <- mobility %>% filter(sub_region_1 == "Texas" & sub_region_2 == "Dallas County")
mobility_Dallas_2021_01_19 <- mobility_Dallas %>% filter(date == "2021-01-19")
p_mobility <- ggplot(mobility_Dallas, aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline)) +
  geom_line() +
  geom_smooth() +
  #geom_vline(xintercept = as.numeric(as.Date("2021-01-19")), linetype = "dashed", color = "red") + 
  #annotate("text", x = as.Date("2021-01-19"), y = max(mobility_Dallas_2021_01_19$retail_and_recreation_percent_change_from_baseline), 
  #         label = cases_Dallas_2021_01_19$retail_and_recreation_percent_change_from_baseline, vjust = 2, hjust = 1.2) +
  # Add a red vertical line
  geom_point(data = mobility_Dallas_2021_01_19, aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline), color = "red", shape = 19, size = 2)  # Add a red marker

p_mobility <- p_mobility +
  geom_point(data = holidays, aes(x = holiday_date, y = mobility_Dallas$grocery_and_pharmacy_percent_change_from_baseline[match(holiday_date, mobility_Dallas$date)]), color = "blue", size = 4) +
  geom_text_repel(data = holidays, aes(x = holiday_date, y = mobility_Dallas$grocery_and_pharmacy_percent_change_from_baseline[match(holiday_date, mobility_Dallas$date)], label = holiday_name), color = "blue", size = 3, hjust = 2, vjust = 3)
#p_mobility <- p_mobility + geom_text_repel(data = holidays, aes(x = holiday_date, y = mobility_Dallas$retail_and_recreation_percent_change_from_baseline[match(holiday_date, mobility_Dallas$date)], label = holiday_name), 
#                        color = "blue", size = 3, box.padding = 0.5, vjust=-3, hjust=1)
p_mobility <- p_mobility + geom_text_repel(data = mobility_Dallas %>% filter(date == "2021-01-19"), aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline, label = paste("Jan 19","Value: ", mobility_Dallas_2021_01_19$grocery_and_pharmacy_percent_change_from_baseline)), 
                         color = "red", size = 3, box.padding = 0.5, vjust=-10, hjust=-1)
p_mobility

# Print both plots
# Print both plots
#grid.arrange(p_cases, p_mobility, ncol = 2)
