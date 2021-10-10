library(tidyverse)
library(janitor)
library(readr)
library(dplyr)

#Qn 1a
airbnb <- read_csv("assign_2.csv")

#airbnb %>%
  #colnames() %>%
  #view()

#Qn 1c renaming variable
airbnb <-
  airbnb %>%
  rename("neighborhood" = "neighbourhood")

#Qn 2a Counting number of each unique observation in column
neighborhoods <- airbnb %>%
  count(neighborhood)

#Qn 2b Identifying the top 20 neighborhoods
neighborhoods <- neighborhoods %>%
  filter(!is.na(neighborhood))  %>%
  arrange(desc(n)) %>%
  head(20)

#Qn 2c Keeping the top 20 neighborhoods in main dataset
airbnb_top_neighborhoods <- airbnb %>%
  filter(neighborhood %in% neighborhoods$neighborhood)

#Qn 2d Creating summary stats
summary_stats_top_neighborhoods <- airbnb_top_neighborhoods %>%
  group_by(neighborhood) %>%
  summarize(
    avg_square_feet = mean(square_feet, na.rm = T),
    avg_price = mean(price, na.rm = T),
    sd_price = sd(price, na.rm = T),
    max_price = max(price, na.rm = T),
    min_price = min(price, na.rm = T)
  ) %>% 
  arrange(desc(avg_square_feet))

#No prices are not decreasing in the summary statistics tibble

#2e Finding highest avg squage feet
highest_avg_square_ft <- summary_stats_top_neighborhoods$avg_square_feet [1]

#2f Finding second highest price
summary_stats_top_neighborhoods <- summary_stats_top_neighborhoods %>%
  arrange(desc(avg_price)) 

second_avg_price <- summary_stats_top_neighborhoods$avg_price [2]