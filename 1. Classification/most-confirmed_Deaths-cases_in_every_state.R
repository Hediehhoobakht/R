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
  group_by(state) %>%
  summarise(Total_Confirmed_Cases = sum(`confirmed_cases`))

# Print the result
print(summation_by_state)


# Create a bar plot of the total confirmed cases by state
ggplot(summation_by_state, aes(x = state, y = Total_Confirmed_Cases),width=2) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Total Confirmed Cases by State",
    x = "State",
    y = "Total Confirmed Cases"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), # Center-align the plot title
    axis.text.x = element_text(angle = 90,hjust =0) # Center-align the x-axis labels
  )
###############################################
# Calculate the summation of confirmed cases by state
summation_by_state <- data %>%
  group_by(state) %>%
  summarise(Total_deaths = sum(`deaths`))

# Print the result
print(summation_by_state)

# Create a bar plot of the total confirmed cases by state
ggplot(summation_by_state, aes(x = state, y = Total_deaths),width=2) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Total deaths by State",
    x = "State",
    y = "Total deaths"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), # Center-align the plot title
    axis.text.x = element_text(angle = 90,hjust =0) # Center-align the x-axis labels
  )

# Calculate the summation of confirmed cases by state
summation_by_state <- data %>%
  group_by(state) %>%
  summarise(Total_Confirmed_Cases = sum(`confirmed_cases`))

# Print the result
print(summation_by_state)

# Create a bar plot of the total confirmed cases by state
ggplot(summation_by_state, aes(x = state, y = Total_Confirmed_Cases),width=2) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Total Confirmed Cases by State",
    x = "State",
    y = "Total Confirmed Cases"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), # Center-align the plot title
    axis.text.x = element_text(angle = 90,hjust =0) # Center-align the x-axis labels
  )