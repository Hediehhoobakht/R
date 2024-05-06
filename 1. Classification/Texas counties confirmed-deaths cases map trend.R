
# load the library package and read the file
library(tidyverse)
library(dplyr)              # Load the dplyr package
library(ggplot2)            # Load the ggplot2 package
library(tibble)
library("ggrepel")
library("ggcorrplot")
library("DT")
library(maps)
library(mapdata)
# Read the COVID-19 cases data
cases <- read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/COVID-19_cases_plus_census.csv")

cases <- cases %>% mutate_if(is.character, factor)
dim(cases)
cases_TX <- cases %>% filter(state == "TX")
dim(cases_TX)
summary(cases_TX[,1:10])
ggplot(cases_TX, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + geom_point()
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX, deaths >= 1000)) 
cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)
cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

head(cases_TX_select)
datatable(cases_TX_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)
ggplot(cases_TX_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))
cor_TX <- cor(cases_TX_select[,-1])
ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_select[,-1]), insig = "blank", hc.order = TRUE)




# Load the county map data for Texas
counties <- as_tibble(map_data("county"))

# Filter the map data for Texas
counties_TX <- counties %>%
  dplyr::filter(region == "texas") %>%
  rename(county = subregion)

# Modify the county names to match the COVID-19 cases data
cases_TX <- cases_TX_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))

counties_TX <- counties_TX %>% left_join(cases_TX %>% 
                                           select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))
#Calculate rates (per 1000 people)
cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)
print(cases_TX_select)
cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

# Create the plot for confirmed cases
ggplot(counties_TX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "COVID-19 Cases per 1000 People", subtitle = "Only counties reporting 100+ cases", hjust = 0)

# Create the plot for deaths cases
ggplot(counties_TX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "COVID-19 deaths Cases per 1000 People", subtitle = "Only counties reporting 100+ cases", hjust = 0)
# Create the plot for deaths cases per cases
ggplot(counties_TX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = death_per_case)) +
  coord_quickmap() + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "COVID-19 deaths per Cases per 1000 People", subtitle = "Only counties reporting 100+ cases", hjust = 0)
