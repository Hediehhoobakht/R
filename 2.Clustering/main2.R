# Load the necessary libraries
library(tidyverse)
library(dplyr)              # Load the dplyr package
library(ggplot2)            # Load the ggplot2 package
library(tibble)
library(openxlsx)
library(dplyr)
library(readxl)
library(maps)
library(mapdata)
# Read the CSV file
data <- read.csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/COVID-19_cases_plus_census.csv")
data <- tibble(data)
# Calculate cases and deaths per population
data <- data %>%
  filter(state == "TX")%>%
  mutate(population=total_pop,
         cases_per_population = 100 * confirmed_cases / total_pop,
         deaths_per_population = 100 * deaths / total_pop,
         bachelors= bachelors_degree,
         master=masters_degree,
         diploma=high_school_diploma,
         associate=associates_degree,
         income= income_per_capita,
         age=median_age,
         nhouseholds=nonfamily_households,
         fhouseholds=family_households,
         commute=commuters_by_public_transportation)

# Create the result data frame with county_name and cases_per_population
result <- data %>%
  select(county_name,population, cases_per_population,
         deaths_per_population,bachelors,master,diploma,associate,
         income,age,nhouseholds,fhouseholds,commute)

new_file <- createWorkbook()

# Add a worksheet to the new Excel file
addWorksheet(new_file, "sheet 1")


# Write the data frame to the new worksheet
writeData(new_file, sheet = "sheet 1", x =result)

# Save the new Excel file
saveWorkbook(new_file, "C:/Users/Hedieh/Documents/SMU/R/Project2/Texas-covid-Data-groups.xlsx")

covid_data <- read_excel("C:/Users/Hedieh/Documents/SMU/R/Project2/Texas-covid-Data-groups.xlsx")
cluster_data <- covid_data %>%
  select(
    county_name, cases_per_population,
     income, age  )

# Perform k-means clustering
k_clusters <- 3  # Adjust the number of clusters as needed
km <- kmeans(cluster_data, centers = k_clusters, nstart = 10)

# Add cluster information to the data
covid_data <- covid_data %>%
  add_column(cluster = factor(km$cluster))

ggplot(covid_data, aes(x =cases_per_population , y = income, color = factor(cluster))) +
  geom_point() +
  ggtitle("Clustering Counties based on Cases per Population") +
  xlab("Cases per Population") +
  ylab("income") +
  scale_color_discrete(name = "Cluster")








##################

subset_data <- select(cluster_1_data, cases_per_population, bachelors)

# Standardize the data
scaled_data <- scale(subset_data)

wss <- numeric(10)

# Iterate over different numbers of clusters
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i)
  wss[i] <- sum(kmeans_model$withinss)
}
elbow_plot <- ggplot(data = tibble(Num_Clusters = 1:10, WSS = wss), aes(x = Num_Clusters, y = WSS)) +
  geom_line() +
  geom_point(shape = 19, size = 2) +
  geom_vline(xintercept = 5, color = "red", linetype = 2) +
  labs(x = "Number of Clusters", y = "Within-cluster Sum of Squares",
       title = "Elbow Method for Optimal Number of Clusters") +
  theme_minimal()
print(elbow_plot)

num_clusters <- 3

# K-means clustering based on cases_per_population and bachelor degree within Cluster 1
km <- kmeans(scaled_data, centers = num_clusters, nstart = 10)

# Add cluster information to the original data
cluster_1_data <- mutate(cluster_1_data, subcluster = factor(km$cluster))

# Visualize the new clusters along with the original population-based clusters
ggplot(cluster_1_data, aes(x = cases_per_population, y = bachelors, color = subcluster, label = county_name)) +
  geom_point() +
  ggtitle("Clustering Counties within Cluster 1 based on Cases per Population and Bachelor Degree")

