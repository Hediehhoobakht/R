# load the library package and read the file
library(tidyverse)
library(dplyr)              # Load the dplyr package
library(ggplot2)            # Load the ggplot2 package
library(tibble)
# read the csv file
data<- read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/COVID-19_cases_TX.csv")
data <- subset(data, select = -c(county_fips_code, state_fips_code, state))
head(data)


library(dplyr)

# Assuming your_data is your dataframe
result <- data %>%
  group_by(county_name) %>%
  filter(confirmed_cases > 0) %>%
  summarise(min_date = min(date))

# Print the result
print(result)


library(openxlsx)

# Assuming your_data is your dataframe
result <- data %>%
  group_by(county_name) %>%
  filter(confirmed_cases > 0) %>%
  summarise(First_reported_case = min(date))


data1<- read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/COVID-19_cases_plus_census.csv")
result1 <- data1 %>%
  group_by(county_name) %>%
  filter(state == "TX") %>%
  summarise(population = total_pop,spread_rate= confirmed_cases/total_pop*1000,)
print(result1)

#result <- data %>%
#  rowwise() %>%
#  mutate(matched_county = stringdist_left_join(county_name, data1$county_name, method = "jw", ignore_case = TRUE)$x) %>%
#  left_join(result, by = c("matched_county" = "county_name")) %>%
#  select(-matched_county)
#result$population <- ifelse(result$county_name == result1$county_name, result1$population, result$population)
result$population <- ifelse(result$county_name %in% result1$county_name, 
                            result1$population[match(result$county_name, result1$county_name)], 
                            NA)
result$spread_rate <- ifelse(result$county_name %in% result1$county_name, 
                            result1$spread_rate[match(result$county_name, result1$county_name)], 
                            NA)
print(result)
result2 <- data1 %>%
  group_by(county_name) %>%
  filter(state == "TX") %>%
  summarise(income = income_per_capita)
print(result2)

result$income <- ifelse(result$county_name %in% result2$county_name, 
                            result2$income[match(result$county_name, result1$county_name)], 
                            NA)
print(result)
# Create a new Excel workbook
wb <- createWorkbook()

# Add the result to a new worksheet in the workbook
addWorksheet(wb, "sheet 1")
writeData(wb, sheet = "sheet 1", x = result)

# Save the Excel file
saveWorkbook(wb, "C:/Users/Hedieh/Documents/SMU/R/Project 1/data/attributes.xlsx")

