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
library("DT")
library(pheatmap)
# Read the CSV file
data <- read.csv("C:/Users/Hedieh/Documents/SMU/R/Project 3/COVID-19_cases_plus_census.csv")
data<-data %>% mutate_if(is.character,factor)
data<-data %>% filter(confirmed_cases>0)
data <- data %>%
  mutate(population=total_pop,
         cases_per_population = 100 * confirmed_cases / total_pop,
         deaths_per_population = 100 * deaths / total_pop,
         death_per_case=deaths/confirmed_cases)

cases_USA<-data%>%  select(state,county_name,total_pop,male_under_5,male_10_to_14,male_15_to_17,
                                                      male_18_to_19,male_20,male_21,male_22_to_24,male_25_to_29,male_30_to_34,male_35_to_39,
                                                      male_40_to_44,male_45_to_49,male_50_to_54,male_55_to_59,male_60_61,male_62_64,male_65_to_66,
                                                      male_67_to_69,male_70_to_74,male_75_to_79,male_80_to_84,male_85_and_over,female_under_5,female_5_to_9,
                                                      female_10_to_14,female_15_to_17,female_18_to_19,female_20, female_21,female_22_to_24,female_25_to_29,female_30_to_34,                                               
                                                      female_35_to_39,female_40_to_44,female_45_to_49,female_50_to_54,female_55_to_59,female_60_to_61,female_62_to_64,
                                                      female_65_to_66,female_67_to_69,female_70_to_74,female_75_to_79,female_80_to_84,
                                                      female_85_and_over,cases_per_population,deaths_per_population,death_per_case)
table(complete.cases(cases_USA))
str(cases_USA)
library(seriation)
cm <- cor(cases_USA %>% select_if(is.numeric) %>% na.omit)
pheatmap(cm, margins = c(14,14))
write.xlsx(cases_USA, "C:/Users/Hedieh/Documents/SMU/R/Project 3/USA_cases.xlsx")

summary(cases_USA)                             
# Assuming your data is stored in a data frame named cases_texas
numeric_columns <- sapply(cases_USA, is.numeric)
numeric_cases_USA <- cases_USA[, numeric_columns]


summary_df <- data.frame(
  Feature_name = names(numeric_cases_USA),
  min = apply(numeric_cases_USA, 2, min),
  median = apply(numeric_cases_USA, 2, median),
  mean = apply(numeric_cases_USA, 2, mean),
  max = apply(numeric_cases_USA, 2, max)
)


# Write the summary statistics to an Excel file
write.xlsx(summary_df, "C:/Users/Hedieh/Documents/SMU/R/Project 3/summary_statistics_selected_USA.xlsx")

#normalization

cases_USA_norm <- cases_USA %>%
  mutate_all(~ .)
# Divide all numeric columns (excluding the first one, i.e., "total_pop") by total_pop
numeric_columns <- names(cases_USA)[sapply(cases_USA, is.numeric)]
cases_USA_norm[, 4:48] <- numeric_cases_USA [, numeric_columns[2:46]] / numeric_cases_USA$total_pop



write.xlsx(cases_USA_norm, "C:/Users/Hedieh/Documents/SMU/R/Project 3/USA_cases_normalization.xlsx")

cases_USA_norm <-
  cases_USA_norm %>% mutate(bad = as.factor(cases_per_population > 7.56))                     
cases_USA_norm  %>%pull(bad)  %>%table()
cases_USA_norm_badrate<-cases_USA_norm %>% group_by(state) %>%
  summarize(bad_pct = sum(bad == TRUE)/n()) %>%
  arrange(desc(bad_pct))
write.xlsx(cases_USA_norm_badrate, "C:/Users/Hedieh/Documents/SMU/R/Project 3/cases_USA_norm_badrate.xlsx")

cases_train <- cases_USA_norm %>% filter(state %in% c("TX", "CA", "FL", "NY"))
cases_train %>% pull(bad) %>% table()

cases_test <-  cases_USA_norm %>% filter(!(state %in% c("TX", "CA", "FL", "NY")))
cases_test %>% pull(bad) %>% table()

counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties
counties_all <- counties %>% left_join(cases_train %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue'))+ggtitle("Training Data")

library(caret)

fit <- cases_train %>%
  train(bad ~ . - county_name - state,
        data = . ,
        #method = "rpart",
        method = "rf",
        #method = "nb",
        trControl = trainControl(method = "cv", number = 10)
  )
fit
varImp(fit)
ggplot(varImp(fit),top=20)+ ggtitle('Variable of Importance')

cases_test <- cases_test %>% na.omit
cases_test$bad_predicted <- predict(fit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue'))+ggtitle("Testing Data")

ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue'))+ggtitle("bad predicted Data")

confusionMatrix(data = cases_test$bad_predicted, ref = cases_test$bad)
