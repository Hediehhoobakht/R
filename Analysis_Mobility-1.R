# Libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggcorrplot)

###### 2020 US Region Mobility ######
us_mobility_2020 <- as_tibble((read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/TX_Mobility.csv")))
# Remove columns #
us_mobility_2020 <- us_mobility_2020 %>% select(-place_id)
us_mobility_2020 <- us_mobility_2020 %>% select(-metro_area)
us_mobility_2020 <- us_mobility_2020 %>% select(-iso_3166_2_code)
# Rename columns #
us_mobility_2020 <- rename(us_mobility_2020, `Country Code` = `country_region_code`)
us_mobility_2020 <- rename(us_mobility_2020, `Country` = `country_region`)
us_mobility_2020 <- rename(us_mobility_2020, `State` = `sub_region_1`)
us_mobility_2020 <- rename(us_mobility_2020, `County` = `sub_region_2`)
us_mobility_2020 <- rename(us_mobility_2020, `FIPS` = `census_fips_code`)
us_mobility_2020 <- rename(us_mobility_2020, `County` = `sub_region_2`)
us_mobility_2020 <- rename(us_mobility_2020, `Date` = `date`)
us_mobility_2020 <- rename(us_mobility_2020, `Retail & Recreation % Delta` = `retail_and_recreation_percent_change_from_baseline`)
us_mobility_2020 <- rename(us_mobility_2020, `Grocery & Pharmacy % Delta` = `grocery_and_pharmacy_percent_change_from_baseline`)
us_mobility_2020 <- rename(us_mobility_2020, `Parks % Delta` = `parks_percent_change_from_baseline`)
us_mobility_2020 <- rename(us_mobility_2020, `Public Transit % Delta` = `transit_stations_percent_change_from_baseline`)
us_mobility_2020 <- rename(us_mobility_2020, `Workplaces % Delta` = `workplaces_percent_change_from_baseline`)
us_mobility_2020 <- rename(us_mobility_2020, `Residential % Delta` = `residential_percent_change_from_baseline`)
# Removing NAs #
us_mobility_2020 <- us_mobility_2020 %>% na.omit(`sub_region_1`)

## Overall US Mobility ##
summary(us_mobility_2020)
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Retail & Recreation % Delta`)) + 
  labs(title = "Movement of People in 2020", subtitle = "US Retail & Recreation") +
  geom_line() + 
  geom_smooth()
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Grocery & Pharmacy % Delta`)) +
  labs(title = "Movement of People in 2020", subtitle = "US Grocery & Pharmacy") +
  geom_line() + 
  geom_smooth()
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Parks % Delta`)) + 
  labs(title = "Movement of People in 2020", subtitle = "US Parks") +
  geom_line() + 
  geom_smooth()
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Public Transit % Delta`)) +
  labs(title = "Movement of People in 2020", subtitle = "US Public Transit") +
  geom_line() +
  geom_smooth()
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Workplaces % Delta`)) + 
  labs(title = "Movement of People in 2020", subtitle = "US Workplaces") +
  geom_line() +
  geom_smooth()
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Residential % Delta`)) + 
  labs(title = "Movement of People in 2020", subtitle = "US Residential") +
  geom_line() +
  geom_smooth()

### Heatmap of US Mobility ###
usa_2020 <- as_tibble(map_data("county"))
usa_2020_cleaned <- usa_2020 %>% rename(c(County = subregion))

## Need to adjust the County values to just have the name ##
us_mobility_2020_cleaned<- us_mobility_2020 %>% 
  mutate(County = County %>% str_to_lower() 
         %>% str_replace('\\s+county\\s*$', '')) 

## Merging our map and mobility on the County ##
usa_2020_cleaned <- usa_2020_cleaned %>% 
  inner_join(us_mobility_2020_cleaned %>% 
               select(c(County, Date,
                        `Retail & Recreation % Delta`,
                        `Grocery & Pharmacy % Delta`,
                        `Parks % Delta`,
                        `Public Transit % Delta`,
                        `Workplaces % Delta`,
                        `Residential % Delta`)))

ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Retail & Recreation % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Retail & Rec")
ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Grocery & Pharmacy % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Grocery & Pharmacy")
ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Parks % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Parks")
ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Public Transit % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Public Transit")
ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Workplaces % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Workplaces")
ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Residential % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Residential Areas")
###### 2021 US Region Mobility ######
us_mobility_2021 <- as_tibble((read_csv("C:/Users/Hedieh/Documents/SMU/R/Project 1/data/TX_Mobility.csv")))
# Remove columns #
us_mobility_2021 <- us_mobility_2021 %>% select(-place_id)
us_mobility_2021 <- us_mobility_2021 %>% select(-metro_area)
us_mobility_2021 <- us_mobility_2021 %>% select(-iso_3166_2_code)
# Rename columns #
us_mobility_2021 <- rename(us_mobility_2021, `Country Code` = `country_region_code`)
us_mobility_2021 <- rename(us_mobility_2021, `Country` = `country_region`)
us_mobility_2021 <- rename(us_mobility_2021, `State` = `sub_region_1`)
us_mobility_2021 <- rename(us_mobility_2021, `County` = `sub_region_2`)
us_mobility_2021 <- rename(us_mobility_2021, `FIPS` = `census_fips_code`)
us_mobility_2021 <- rename(us_mobility_2021, `County` = `sub_region_2`)
us_mobility_2021 <- rename(us_mobility_2021, `Date` = `date`)
us_mobility_2021 <- rename(us_mobility_2021, `Retail & Recreation % Delta` = `retail_and_recreation_percent_change_from_baseline`)
us_mobility_2021 <- rename(us_mobility_2021, `Grocery & Pharmacy % Delta` = `grocery_and_pharmacy_percent_change_from_baseline`)
us_mobility_2021 <- rename(us_mobility_2021, `Parks % Delta` = `parks_percent_change_from_baseline`)
us_mobility_2021 <- rename(us_mobility_2021, `Public Transit % Delta` = `transit_stations_percent_change_from_baseline`)
us_mobility_2021 <- rename(us_mobility_2021, `Workplaces % Delta` = `workplaces_percent_change_from_baseline`)
us_mobility_2021 <- rename(us_mobility_2021, `Residential % Delta` = `residential_percent_change_from_baseline`)
# Removing NAs #
us_mobility_2021 <- us_mobility_2021 %>% na.omit(`sub_region_1`)

## Overall US Mobility ##
summary(us_mobility_2021)
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Retail & Recreation % Delta`)) + 
  labs(title = "Movement of People in 2021", subtitle = "US Retail & Recreation") +
  geom_line() + 
  geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Grocery & Pharmacy % Delta`)) + 
  labs(title = "Movement of People in 2021", subtitle = "US Grocery & Pharmacy") +
  geom_line() + 
  geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Parks % Delta`)) +
  labs(title = "Movement of People in 2021", subtitle = "US Parks") +
  geom_line() +
  geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Public Transit % Delta`)) +
  labs(title = "Movement of People in 2021", subtitle = "US Public Transit") +
  geom_line() +
  geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Workplaces % Delta`)) +
  labs(title = "Movement of People in 2021", subtitle = "US Workplaces") +
  geom_line() +
  geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Residential % Delta`)) +
  labs(title = "Movement of People in 2021", subtitle = "US Residential") +
  geom_line() +
  geom_smooth()

### Heatmap of US Mobility ###
usa_2021 <- as_tibble(map_data("county"))
usa_2021_cleaned <- usa_2021 %>% rename(c(County = subregion))

## Need to adjust the County values to just have the name ##
us_mobility_2021_cleaned<- us_mobility_2021 %>% 
  mutate(County = County %>% str_to_lower() 
         %>% str_replace('\\s+county\\s*$', '')) 

## Merging our map and mobility on the County ##
usa_2021_cleaned <- usa_2021_cleaned %>% 
  inner_join(us_mobility_2021_cleaned %>% 
               select(c(County, Date,
                        `Retail & Recreation % Delta`,
                        `Grocery & Pharmacy % Delta`,
                        `Parks % Delta`,
                        `Public Transit % Delta`,
                        `Workplaces % Delta`,
                        `Residential % Delta`)))

ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Retail & Recreation % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Retail & Rec")
ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Grocery & Pharmacy % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Grocery & Pharmacy")
ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Parks % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Parks")
ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Public Transit % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Public Transit")
ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Workplaces % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Workplaces")
ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Residential % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Residential Areas")
###### 2022 US Region Mobility ######
us_mobility_2022 <- as_tibble((read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/2021_US_Region_Mobility_Report.csv")))
# Remove columns #
us_mobility_2022 <- us_mobility_2022 %>% select(-place_id)
us_mobility_2022 <- us_mobility_2022 %>% select(-metro_area)
us_mobility_2022 <- us_mobility_2022 %>% select(-iso_3166_2_code)
# Rename columns #
us_mobility_2022 <- rename(us_mobility_2022, `Country Code` = `country_region_code`)
us_mobility_2022 <- rename(us_mobility_2022, `Country` = `country_region`)
us_mobility_2022 <- rename(us_mobility_2022, `State` = `sub_region_1`)
us_mobility_2022 <- rename(us_mobility_2022, `County` = `sub_region_2`)
us_mobility_2022 <- rename(us_mobility_2022, `FIPS` = `census_fips_code`)
us_mobility_2022 <- rename(us_mobility_2022, `County` = `sub_region_2`)
us_mobility_2022 <- rename(us_mobility_2022, `Date` = `date`)
us_mobility_2022 <- rename(us_mobility_2022, `Retail & Recreation % Delta` = `retail_and_recreation_percent_change_from_baseline`)
us_mobility_2022 <- rename(us_mobility_2022, `Grocery & Pharmacy % Delta` = `grocery_and_pharmacy_percent_change_from_baseline`)
us_mobility_2022 <- rename(us_mobility_2022, `Parks % Delta` = `parks_percent_change_from_baseline`)
us_mobility_2022 <- rename(us_mobility_2022, `Public Transit % Delta` = `transit_stations_percent_change_from_baseline`)
us_mobility_2022 <- rename(us_mobility_2022, `Workplaces % Delta` = `workplaces_percent_change_from_baseline`)
us_mobility_2022 <- rename(us_mobility_2022, `Residential % Delta` = `residential_percent_change_from_baseline`)
# Removing NAs #
us_mobility_2022 <- us_mobility_2022 %>% na.omit(`sub_region_1`)

## Overall US Mobility ##
summary(us_mobility_2022)
ggplot(us_mobility_2022, mapping = aes(x = Date, y = `Retail & Recreation % Delta`)) + 
  labs(title = "Movement of People in 2022", subtitle = "US Retail & Recreation") +
  geom_line() + 
  geom_smooth()
ggplot(us_mobility_2022, mapping = aes(x = Date, y = `Grocery & Pharmacy % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2022, mapping = aes(x = Date, y = `Parks % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2022, mapping = aes(x = Date, y = `Public Transit % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2022, mapping = aes(x = Date, y = `Workplaces % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2022, mapping = aes(x = Date, y = `Residential % Delta`)) + geom_line() + geom_smooth()

### Heatmap of US Mobility ###
usa_2022 <- as_tibble(map_data("county"))
usa_2022_cleaned <- usa_2022 %>% rename(c(County = subregion))

## Need to adjust the County values to just have the name ##
us_mobility_2022_cleaned<- us_mobility_2022 %>% 
  mutate(County = County %>% str_to_lower() 
         %>% str_replace('\\s+county\\s*$', '')) 

## Merging our map and mobility on the County ##
usa_2022_cleaned <- usa_2022_cleaned %>% 
  inner_join(us_mobility_2022_cleaned %>% 
               select(c(County, Date,
                        `Retail & Recreation % Delta`,
                        `Grocery & Pharmacy % Delta`,
                        `Parks % Delta`,
                        `Public Transit % Delta`,
                        `Workplaces % Delta`,
                        `Residential % Delta`)))

ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Retail & Recreation % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Retail & Rec")
ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Grocery & Pharmacy % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Grocery & Pharmacy")
ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Parks % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Parks")
ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Public Transit % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Public Transit")
ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Workplaces % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Workplaces")
ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Residential % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Residential Areas")