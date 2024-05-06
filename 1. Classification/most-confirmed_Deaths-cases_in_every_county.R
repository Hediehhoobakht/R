# load the library package and read the file
library(tidyverse)
library(dplyr)              # Load the dplyr package
library(ggplot2)            # Load the ggplot2 package
library(tibble)
data<- read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/COVID-19_cases_plus_census.csv")

as_tibble(data)
glimpse(data)
summary(as_tibble(data))

data <- data%>%
  mutate(
    `confirmed_cases` = as.numeric(`confirmed_cases`),
    `deaths` = as.numeric(`deaths`),
  )
# Calculate the summation of confirmed cases by state
summation_by_state <- data %>%
  group_by(county_name) %>%
  filter(state == "TX") %>%
  filter(confirmed_cases>50000) %>%
  summarise(Total_Confirmed_Cases = (`confirmed_cases`))

# Print the result
print(summation_by_state)


ggplot(summation_by_state, aes(x = Total_Confirmed_Cases, y = county_name)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Total_Confirmed_Cases), hjust = 1, vjust = 0.1, color = "black", size = 5) +
  labs(
    title = "Total Confirmed Cases by County in Texas",
    x = "Total Confirmed Cases",
    y = "County"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), # Center-align the plot title
    axis.text.y = element_text(size = 10), # Adjust text size for y-axis labels
    axis.text.x = element_text(size = 10), # Adjust text size for x-axis labels
  )
###############################################
# Calculate the summation of confirmed cases by state
summation_by_state <- data %>%
  group_by(county_name) %>%
  filter(state == "TX") %>%
  filter(deaths >1000) %>%
  summarise(Total_deaths = (`deaths`))

# Print the result
print(summation_by_state)

# Create a bar plot of the total confirmed cases by state
ggplot(summation_by_state, aes(x =Total_deaths , y = county_name)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Total_deaths), hjust = 1, vjust = 0.1, color = "black", size = 5) +
  labs(
    title = "Total deaths Cases by County in Texas",
    x = "Total deaths Cases",
    y = "County"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), # Center-align the plot title
    axis.text.y = element_text(size = 10), # Adjust text size for y-axis labels
    axis.text.x = element_text(size = 10), # Adjust text size for x-axis labels
  )

# 
