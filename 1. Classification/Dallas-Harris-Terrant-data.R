# Load the necessary libraries
library(tidyverse)
library(dplyr)              # Load the dplyr package
library(ggplot2)            # Load the ggplot2 package
library(tibble)
library(openxlsx)
library(dplyr)

# Read the CSV file
data <- read.csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/COVID-19_cases_plus_census.csv")

# Calculate cases and deaths per population
data <- data %>%
  filter(state == "TX")%>%
  mutate(population=total_pop,
         cases_per_population = 100 * confirmed_cases / total_pop,
         deaths_per_population = 100 * deaths / total_pop,
         average_cases=mean(100 * confirmed_cases / total_pop),
         max_cases=max(100 * confirmed_cases / total_pop),
         average_deaths_per_population = mean(100 * deaths / total_pop),
         Max_deaths_per_population = max(100 * deaths / total_pop))

# Create the result data frame with county_name and cases_per_population
result <- data %>%
  select(county_name,population, cases_per_population,deaths_per_population,average_cases,average_deaths_per_population,max_cases,Max_deaths_per_population)


head(result)
result_filtered <- result %>%
  filter(county_name %in% c("Dallas County", "Harris County","Tarrant County","Borden County", "Loving County", "King County", "Motley County", "Lamb County" , "Cottle County"))
print(result_filtered)
new_file <- createWorkbook()

# Add a worksheet to the new Excel file
addWorksheet(new_file, "sheet 1")


# Write the data frame to the new worksheet
writeData(new_file, sheet = "sheet 1", x =result_filtered)

# Save the new Excel file
saveWorkbook(new_file, "C:/Users/Hedieh/Documents/SMU/R/Project 1/data/Dallas-Harris-Tarrant-Data.xlsx")
