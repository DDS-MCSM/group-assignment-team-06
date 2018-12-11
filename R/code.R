#' Title
#'
#' @details This are the details
#' @return
#' @export
#'
#' @examples
sample_function <- function() {
  print("Hello world")
}

# Libraries
library(dplyr)
library(ggplot2)
library(maps)
library(ggthemes)

filepath <- 'dataset/marx-geo.csv'

dataset <- read.csv(file=filepath,
                    header=TRUE,
                    sep=',',
                    check.names=TRUE,
                    stringsAsFactors = FALSE,
                    na.strings=c(""))

useful_vars <- c("datetime","country","latitude","longitude")
dataset_small <- dataset[useful_vars]

# Remove NAs from coordinates
clean_coordinates <- data.frame(dataset_small %>% filter(!is.na(latitude) & !is.na(longitude)))

# Group coordinates
grouped_coordinates <- data.frame(clean_coordinates %>% group_by(longitude,latitude) %>% summarize(times=n()))

# Remove over range coordinates LAT[-90:90] LONG [-180:180]
cc2 <- data.frame(grouped_coordinates %>% filter(latitude<=90))
cc2 <- data.frame(cc2 %>% filter(latitude>=-90))
cc2 <- data.frame(cc2 %>% filter(longitude<=180))
cc2 <- data.frame(cc2 %>% filter(longitude>=-180))

# Remove NAs from countries
clean_countries <- data.frame(dataset_small %>% filter(!is.na(country)))

# Group countries
grouped_countries <- data.frame(clean_countries %>% group_by(country) %>% summarize(times=n()))

# Rewrite some countries
world_data <- map_data('world')
grouped_countries <- grouped_countries %>% mutate(country = if_else(country == "United States", 'USA',
                                        if_else(country == "United Kingdom", 'UK', country)))

# World map
world <- ggplot() + borders("world", colour="#D7D7D7", fill="#CBCBCB") + theme_map()

# Coordinates map
map_coordinates <- world +
  geom_point(aes(x = longitude, y = latitude, size = times), data = cc2, colour = 'blue', alpha = .25)

# Countries map

