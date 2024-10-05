library(readr)

getwd()
setwd("/Users/dhrithireddy/Documents")
dem_score <- read_csv("https://moderndive.com/data/dem_score.csv")
dem_score

View(dem_score)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(nycflights13)
install.packages('fivethirtyeight')
library(fivethirtyeight)

View(drinks)

drinks_smaller <- drinks %>% 
  filter(country %in% c("USA", "China", "Italy", "Saudi Arabia")) %>% 
  select(-total_litres_of_pure_alcohol) %>% 
  rename(beer = beer_servings, spirit = spirit_servings, wine = wine_servings)
drinks_smaller

drinks_smaller_tidy <- drinks_smaller %>% 
  pivot_longer(names_to = "type", 
               values_to = "servings", 
               cols = -country)
drinks_smaller_tidy

ggplot(drinks_smaller_tidy, aes(x = country, y = servings, fill  = type)) +
  geom_col(position = "dodge")

#position = "dodge" gives side by side chart

?pivot_longer



guat_dem <- dem_score %>% 
  filter(country == "Guatemala")
guat_dem

guat_dem_tidy <- guat_dem %>% 
  pivot_longer(names_to = "year", 
               values_to = "democracy_score", 
               cols = -country,
               names_transform = list(year = as.integer)) 

guat_dem_tidy
