library(tidyverse)
library(factoextra)
library(cluster)
library(fpc)
library(dbscan)
library(gridExtra)
covid_data<- read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/COVID-19_cases_plus_census.csv")
covid_data <- as_tibble(covid_data)
covid_data <- covid_data %>%
  group_by(county_name) %>%
  filter(state == "TX") %>%
  summarise(population = total_pop,
            cases_per_population = 100 * confirmed_cases / total_pop,
            deaths_per_population = 100 * deaths / total_pop,
            bach= bachelors_degree,income= income_per_capita,
            age=median_age,
            nhouseholds=nonfamily_households,
            fhouseholds=family_households,
            commute=commuters_by_public_transportation)
print(covid_data)
summary(covid_data)
## I use this till tidyverse implements a scale function
scale_numeric <- function(x) mutate_if(x, is.numeric, function(y) as.vector(scale(y)))

covid_data_scaled <- covid_data |> 
  scale_numeric()
summary(covid_data_scaled)
# Select only numeric columns
covid_data_scaled <- covid_data_scaled[, sapply(covid_data_scaled, is.numeric)]
#################### scaling
ggplot(covid_data_scaled, aes(x =population , y =bach )) + geom_point()

#########################
library(GGally)
# Select the features you want to include in the scatterplot matrix
selected_features <- c("population", "age","bach", "nhouseholds", "income")

# Subset the data frame with selected features
subset_data <- covid_data_scaled[selected_features]

# Create a scatterplot matrix
ggpairs(subset_data, progress = FALSE)

##############
km <- kmeans(covid_data_scaled, centers = 4, nstart = 10)
covid_data_scaled_km <- covid_data_scaled|>
  add_column(cluster = factor(km$cluster))
centroids <- as_tibble(km$centers, rownames = "cluster")

ggplot(covid_data_scaled_km, aes(x =population , y =population, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x =population , y =population , color = cluster), shape = 3, size = 10)
################
lof <- lof(covid_data_scaled, minPts= 10)
lof
ggplot(tibble(index = seq_len(length(lof)), lof = sort(lof)), aes(index, lof)) +
  geom_line() +
  geom_hline(yintercept = 1.0, color = "red", linetype = 2)+geom_hline(yintercept = 1.5, color = "blue", linetype = 2)

ggplot(covid_data_scaled |> add_column(lof = lof), aes(population, age, color = lof)) +
  geom_point() + scale_color_gradient(low = "gray", high = "red")+ggtitle("outliers based on age vs. Cases per Population") +
  xlab("Cases per Population") +
  ylab("age") 

ggplot(tibble(index = seq_len(length(lof)), lof = sort(lof)), aes(index, lof)) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red", linetype = 2)

ggplot(covid_data_scaled |> add_column(outlier = lof >= 2), aes(population, bach, color = outlier)) +
  geom_point()
######################
#removing outliers
covid_data_scaled_clean <- covid_data_scaled  |> filter(lof < 1.5)
selected_features <- c("population", "age","bach", "nhouseholds", "income")

# Subset the data frame with selected features
subset_data <- covid_data_scaled_clean[selected_features]

# Create a scatterplot matrix
ggpairs(subset_data, progress = FALSE)

scale_numeric <- function(x) mutate_if(x, is.numeric, function(y) as.vector(scale(y)))

covid_data_scaled_clean_scaled <- covid_data_scaled_clean |> 
  scale_numeric()

summary(covid_data_scaled_clean_scaled)

set.seed(123)
ks <- 2:10
WCSS <- sapply(ks, FUN = function(k) {
  kmeans(covid_data_scaled_clean_scaled, centers = k, nstart = 5)$tot.withinss
})

ggplot(tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 4, color = "red", linetype = 2)

d <- dist(covid_data_scaled_clean_scaled)
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(covid_data_scaled_clean_scaled, centers=k, nstart = 5)$cluster)$avg.silwidth
})

best_k <- ks[which.max(ASW)]
best_k
ggplot(tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = best_k, color = "red", linetype = 2)


#############
km <- kmeans(covid_data_scaled_clean_scaled, centers = 4, nstart = 10)
covid_data_scaled_clean_km <- covid_data_scaled_clean_scaled|>
  add_column(cluster = factor(km$cluster))
centroids <- as_tibble(km$centers, rownames = "cluster")

plot1 <-ggplot(covid_data_scaled_clean_km, aes(population, bach, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(population, bach, color = cluster), shape = 3, size = 10)
plot2 <-ggplot(covid_data_scaled_clean_km, aes(population, age, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(population, age, color = cluster), shape = 3, size = 10)
plot3 <-ggplot(covid_data_scaled_clean_km, aes(population, income, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(population, income, color = cluster), shape = 3, size = 10)
plot4 <-ggplot(covid_data_scaled_clean_km, aes(population, nhouseholds, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(population, nhouseholds, color = cluster), shape = 3, size = 10)


# Display the plots side by side

grid.arrange(plot1, plot2,plot3,plot4, ncol = 2, nrow=2)




###################
centroid_plot_data <- pivot_longer(centroids, cols = c(population, age, bach, nhouseholds, income,cases_per_population), names_to = "feature")
print(centroid_plot_data)
ggplot(pivot_longer(centroids, cols = c(population,age,bach,nhouseholds, income,cases_per_population), names_to = "feature"),
       aes(x = value, y = feature, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))
##################
#hierarchial

# Assuming your original code
d <- dist(covid_data_scaled_clean)
hc <- hclust(d, method = "complete")

# Plotting the dendrogram
plot(hc, xlab = "Observations", ylab = "Height", main = "Hierarchical Clustering Dendrogram", labels = rownames(d), las = 2, lwd = 5, font = 10, cex = 5,cex.lab =10, cex.axis = 10) 

# Assuming you have already defined and computed your variables
d <- dist(covid_data_scaled_clean)
hc <- hclust(d, method = "complete")
fviz_dend(hc, k = 4)
# Setting up a graphical device with a larger width
dev.new(width = 10, height = 6)

# Plotting the dendrogram with adjusted parameters
plot(hc, xlab = "", ylab = "Height", main = "Hierarchical Clustering Dendrogram", labels = rownames(d), las = 2, cex = 0.6, cex.lab = 1.5, cex.axis = 1.2, hang = -1,font = 2)

# Adding a title for the x-axis
mtext("Observations", side = 1, line = 3, cex = 1.5)

# Adding a title for the y-axis
mtext("Height", side = 2, line = 3, cex = 1.5)

# Adjusting the margin for better aesthetics
par(mar = c(5, 5, 4, 2) + 0.1)

# Additional plot with hang parameter
plot(hc, hang = -1, labels = rownames(d), cex = 1.0, cex.lab = 1.5, cex.axis = 1.2)

fviz_dend(hc, k = 4)

##########
# Select the first quarter of your data
quarter_data <- covid_data_scaled_clean[1:(nrow(covid_data_scaled_clean) / 4), ]

# Compute the distance matrix
d <- dist(quarter_data)

# Perform hierarchical clustering
hc <- hclust(d, method = "complete")

# Plot the dendrogram
par(mar = c(5, 5, 2, 10))  # Adjusting margin for better layout
plot(hc, xlab = "Observations", ylab = "Height", main = "Hierarchical Clustering Dendrogram (First Quarter)", labels = rownames(d), las = 2)
fviz_dend(hc, k = 4)
#########################
d <- dist(covid_data_scaled_clean_scaled)
km <- kmeans(covid_data_scaled_clean_scaled, centers = 4, nstart = 10)
table(km$cluster)
cluster_stats <- cluster.stats(d, km$cluster)

# Print the results
print(cluster_stats)
library(fpc)
fpc::cluster.stats(d, km$cluster)
hc_single <- hclust(d, method = "single")
silhouette_values <- silhouette(km$cluster, dmatrix = as.matrix(d))
print(silhouette_values)

# Assuming silhouette_values is a vector returned by silhouette function
silhouette_values <- silhouette(km$cluster, dmatrix = as.matrix(d))

# Convert the vector to a data frame or list
# Example using a data frame
silhouette_values_df <- data.frame(sil_width = silhouette_values)

# Now you can access the sil_width column using the $ operator
print(silhouette_values_df)
average_silhouette_width <- mean(silhouette_values_df$sil_width.sil_width)
print(average_silhouette_width)
fviz_silhouette(silhouette(km$cluster, d))
