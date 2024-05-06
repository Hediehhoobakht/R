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
  select(county_name,population, cases_per_population,deaths_per_population,bachelors,master,diploma,associate)


head(result)

new_file <- createWorkbook()

# Add a worksheet to the new Excel file
addWorksheet(new_file, "sheet 1")


# Write the data frame to the new worksheet
writeData(new_file, sheet = "sheet 1", x =result)

# Save the new Excel file
saveWorkbook(new_file, "C:/Users/Hedieh/Documents/SMU/R/Project2/Texas-covid-Data.xlsx")

covid_data <- read_excel("C:/Users/Hedieh/Documents/SMU/R/Project2/Texas-covid-Data.xlsx")
# K-means clustering based on cases_per_population
km <- kmeans(covid_data["cases_per_population"], centers = 3, nstart = 10)

# Add cluster information to the covid_data
covid_data <- covid_data %>%
  add_column(cluster = factor(km$cluster))

# Visualize clusters
ggplot(covid_data, aes(x = population, y = cases_per_population, color = cluster)) +
  geom_point() +
  ggtitle("Clustering Counties based on Cases per Population")
# Save the updated Excel file with cluster information
write.xlsx(covid_data, "C:/Users/Hedieh/Documents/SMU/R/Project2/Texas-covid-Data_with_clusters.xlsx")

# Specify the counties you want to label
counties_to_label <- c("Harris County", "Bexar County", "Tarrant County", "Dallas County")

# Visualize clusters and label specific counties
ggplot(covid_data, aes(x = population, y = cases_per_population, color = cluster, label = county_name)) +
  geom_point() +
  geom_text(data = filter(covid_data, county_name %in% counties_to_label),
            aes(label = county_name), vjust = -0.3, hjust = 1, size = 3, color = "black") +
  ggtitle("Clustering Counties based on Cases per Population")
cluster_counts <- table(covid_data$cluster)
print(cluster_counts)
#############

# Create a tibble of US county map data
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))

# Modify the mutation line to ensure the new column is named 'county'
covid_data <- covid_data %>%
  mutate(county = county_name %>% 
           str_to_lower() %>% 
           str_replace('\\s+county\\s*$', '')) %>%
  select(county, cluster)  # Select the columns you need

counties_TX_clust <- counties_TX %>% left_join(covid_data %>% 
                                                 add_column(cluster1 = factor(km$cluster)))

# Create a map plot
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster1)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")


