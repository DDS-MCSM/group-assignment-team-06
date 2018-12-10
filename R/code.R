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

filepath <- 'dataset/marx-geo.csv'

dataset <- read.csv(file=filepath,
           header=TRUE,
           sep=',',
           check.names=TRUE,
           stringsAsFactors = FALSE,
           na.strings=c(""))

useful_vars <- c("datetime","latitude","longitude")
dataset_small <- dataset[useful_vars]

# Remove NAs
clean_coordinates <- data.frame(dataset_small %>% filter(!is.na(latitude) & !is.na(longitude)))

# Remove over range coordinates LAT[-90:90] LONG [-180:180]
cc2 <- data.frame(clean_coordinates %>% filter(latitude<=90))
cc2 <- data.frame(cc2 %>% filter(latitude>=-90))
cc2 <- data.frame(cc2 %>% filter(longitude<=180))
cc2 <- data.frame(cc2 %>% filter(longitude>=-180))
