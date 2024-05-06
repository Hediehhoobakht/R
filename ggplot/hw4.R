library(tidyverse)
mlb <- read_csv("C:/Users/Hedieh/Downloads/MLB_cleaned.csv")
#It appears that program has successfully read my CSV file using read_csv(), 
#and the column types have been automatically inferred based on the data. 
#The message indicates that the data frame mlb has been created with the correct
#column types so it does not need to define the data structure for it, but I will do for the purpose of learning

library(tibble)
as_tibble(mlb)
glimpse(mlb)
summary(as_tibble(mlb))
library(dplyr)
mlb <- mlb %>%
  mutate(
    `Height(inches)` = as.numeric(`Height(inches)`),
    `Weight(pounds)` = as.numeric(`Weight(pounds)`),
    Age = as.numeric(Age)
  )

mlb <- mlb %>%
  mutate(
    `First Name` = factor(`First Name`),
    `Last Name` = factor(`Last Name`),
    Team = factor(Team),
    Position = factor(Position)
  )
summary(as_tibble(mlb))
#2.Select only the players for the team 'ARZ'. Compare the column Team with 'ARZ' and use subsetting to select the rows.
mlb_filtered <- mlb %>%
  filter(Team == "ARZ")
print(mlb_filtered)
#3.How many players does the team 'ARZ' have in the data set?
mlb %>%
  filter(Team == "ARZ") %>%
  nrow()
result <- mlb %>%
  summarise(Count = sum(Team == "ARZ"))
#4.What is the weight of the heaviest player of the team 'ARZ' (use a function).
mlb %>%
  filter(Team == "ARZ") %>%
  summarise(MaxWeight = max(`Weight(pounds)`))
#5.What is the average age of all players in the dataset?
mlb %>%
  summarise(MeanAge = mean(Age, na.rm = TRUE))
#6.Add a column called BMI and add the body mass indexLinks to an external site..
mlb <- mlb %>%
  mutate(
    BMI = (`Weight(pounds)` / (`Height(inches)` ^ 2)) * 703
  )
as_tibble(mlb)
#7.Create a tibble containing the names, year of birth, the month of birth, and day of birth as 
#separate columns with the information for 3 people.
#Make sure the tibble has column names (see colnames()).

# Create a tibble with the specified columns
people <- tibble(
  Name = c("Hedieh", "Sobhan", "Kimia"),
  YearOfBirth = c(1991, 1991, 1997),
  MonthOfBirth = c(8, 5, 7),
  DayOfBirth = c(19, 17, 26)
)

# View the tibble
print(people)
#8.Write the tibble to a file in CSV format and check it in Excel.
write.csv(people, file = "C:/Users/Hedieh/Documents/SMU/R/hw4/people.csv", row.names = FALSE)
#########################

##Base R Plots
#1.Plot a sin(x)/x. Hint: Trigonometric functions in R use angles in radians (see 'sin')
# Create a sequence of x values
x <- seq(-10, 10, by = 0.01)

# Calculate the corresponding y values
y <- sin(x)/x

# Create the plot
plot(x, y, type = "l", main = "Plot of sin(x)/x in R-base", xlab = "x", ylab = "f(x)")
#2.The "cars" data set gives the speed of cars and the distances taken to stop. 
#Note that the data were recorded in the 1920s. Plot the "cars" data set as a scatter plot.
#Plot all data points with distances taken to stop greater than 80 in red. 
#The "cars" dataset is included in the R installation (in package datasets) and can be loaded with the function data().
# Load the "cars" dataset
data("cars")
car1<-as_tibble(cars)
print(car1)
# Create a scatter plot of the entire dataset
plot(cars, main = "Scatter Plot of Cars Data-Rbase", xlab = "Speed", ylab = "Stopping Distance", pch = ifelse(cars$dist > 80, 19, 19), col = ifelse(cars$dist > 80, "red", "black"))
#3.Plot histograms for speed and dist in "cars".
# Load the "cars" dataset
data("cars")

# Create a histogram for the "speed" variable
hist_speed <-hist(cars$speed, 
     main = "Histogram of Speed",
     xlab = "Speed",
     ylab = "Frequency",
     col = "blue",          # Set the bar color to blue
     border = "black",      # Set the border color of bars to black
     xlim = c(0, max(cars$speed)))  # Set the x-axis limits
     
text(hist_speed$mids, hist_speed$counts, labels = hist_speed$counts, pos = 3, col = "black")


# Create a histogram for the "dist" variable
hist_dist <- hist(cars$dist, 
                  main = "Histogram of Stopping Distance",
                  xlab = "Stopping Distance",
                  ylab = "Frequency",
                  col = "green",         # Set the bar color to green
                  border = "black",      # Set the border color of bars to black
                  xlim = c(0, max(cars$dist)))  # Set the x-axis limits
text(hist_dist$mids, hist_dist$counts, labels = hist_dist$counts, pos = 3, col = "black")
########################################
##Visualizing Data with ggplot2
#1.Plot a sin(x)/x.
# Load the ggplot2 library
library(ggplot2)

# Create a sequence of x values
x <- seq(-10, 10, by = 0.01)

# Calculate the corresponding y values
y <- sin(x)/x

# Create a data frame with x and y
data <- data.frame(x = x, y = y)

# Create the plot
plot <- ggplot(data, aes(x, y)) +
  geom_line() +
  labs(title = "Plot of sin(x)/x in ggplot",
       x = "x",
       y = "f(x)") +
  theme_minimal()

# Print the plot
print(plot)


#2.The "cars" data set gives the speed of cars and the distances taken to stop. 
#Note that the data were recorded in the 1920s. Plot the "cars" data set as a scatter plot. 
#Plot all data points with distances taken to stop greater than 80 in red. Note: load the dataset with the function data().

# Load the "cars" dataset
data("cars")
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  
  # Add red points for distances > 80
  geom_point(data = subset(cars, dist > 80), aes(x = speed, y = dist), color = 'red') +
  
  labs(title = "Scatter Plot of Cars Data-ggplot",
       x = "Speed",
       y = "Stopping Distance") 

#3.Plot histograms for speed and dist in 'cars'.
# Create a scatter plot
ggplot(data = cars, aes(x = speed)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", boundary=0) +
  labs(title = "Histogram of speed-ggplot", x = "speed", y = "Frequency")


ggplot(data = cars, aes(x = dist)) +
  geom_histogram(binwidth = 20, fill = "green", color = "black", boundary=0) +
  labs(title = "Histogram of Distance-ggplot", x = "Distance", y = "Frequency")
