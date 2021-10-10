#ps 2 good luck
library("tidyverse")
library("janitor")
library("readr")
library("dplyr")
library("magrittr")
#1 a
airbnb <- read_csv("assign_2.csv")
#1 b
colnames(airbnb)
#1 c
airbnb %<>% rename("neighborhood" = "neighbourhood")
colnames(airbnb)
#2 a
neighborhoods <- airbnb %>% count(neighborhood)
#2 b
neighborhoods  %<>% filter(!is.na(neighborhood)) %>% arrange(desc(n)) %>% head(20)
#2 c
airbnb_top_neighborhoods <-
  airbnb %>% filter(neighborhood %in% neighborhoods$neighborhood)
#2 d
summary_stats_top_neighborhoods <-
  airbnb_top_neighborhoods %>% group_by(neighborhood) %>%
  summarise(
    avg_square_feet = mean(square_feet, na.rm = T),
    avg_price = mean(price,
                     na.rm = T),
    sd_price = sd(price, na.rm = T),
    max_price = max(price, na.rm = T),
    min_price = min(price, na.rm = T)
  ) %>% arrange(desc(avg_square_feet))
#2 e
highest_avg_square_ft <- c(max(summary_stats_top_neighborhoods$avg_square_feet,na.rm = T))
print(highest_avg_square_ft)
#2 f
summary_stats_top_neighborhoods <- summary_stats_top_neighborhoods %>%
  arrange(desc(avg_price))
second_avg_price <- c(summary_stats_top_neighborhoods$avg_price[2])
print(second_avg_price)