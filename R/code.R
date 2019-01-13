#' Load Dependencies
#'
#' @return
#' @export
#'
#' @examples
loadDependencies <- function() {
  library(dplyr)
  library(rworldmap)
}

#' Download dataset
#'
#' @return
#' @export
#'
#' @examples
downloadDataset <- function() {
  file_url <- "http://datadrivensecurity.info/blog/data/2014/01/marx-geo.tar.gz"
  compressed_file <- "../dataset/marx-geo.tar.gz"
  if(!file.exists(compressed_file)) {
    download.file(file_url,compressed_file)
  }
  file_name <- "../dataset/marx-geo.csv"
  if (!file.exists(file_name)) {
    untar(compressed_file,exdir = '../dataset')
  }
  dataset <- read.csv(file_name,na.strings=c("","NA"))
  return(dataset)
}

#' Clean dataset
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
cleanDataset <- function(dataset) {
  useful_vars <- c("datetime","cc","country","latitude","longitude")
  clean_dataset <- dataset[useful_vars]
  clean_dataset$datetime <- as.Date(clean_dataset$datetime, format = "%Y-%m-%d")
  clean_dataset <- data.frame(clean_dataset %>% filter(!is.na(latitude) & !is.na(longitude)))
  clean_dataset <- data.frame(clean_dataset %>% filter(!is.na(cc)))
  clean_dataset <- data.frame(clean_dataset %>% filter(latitude<=90))
  clean_dataset <- data.frame(clean_dataset %>% filter(latitude>=-90))
  clean_dataset <- data.frame(clean_dataset %>% filter(longitude<=180))
  clean_dataset <- data.frame(clean_dataset %>% filter(longitude>=-180))
  return(clean_dataset)
}

#' Group coordinates
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
groupCoordinates <- function(dataset) {
  coordinates_dataset <- data.frame(dataset %>% group_by(longitude,latitude) %>% summarize(attacks=n()))
  return(coordinates_dataset)
}

#' Group countries
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
groupCountries <- function(dataset) {
  countries_dataset <- data.frame(dataset %>% group_by(cc) %>% summarize(attacks=n()))
  population <- getPopulation()
  countries_population_dataset <- merge(countries_dataset, population, by="cc")
  countries_population_dataset$attacks_population <- countries_population_dataset$attacks/countries_population_dataset$population
  return(countries_population_dataset)
}

#' Get Population Dataframe
#'
#' @return
#' @export
#'
#' @examples
getPopulation <- function() {
  world_data <- getMap()
  population <- setNames(data.frame(world_data$ISO_A2,world_data$NAME,world_data$POP_EST),c('cc','country','population'))
  population <- subset(population,population>0 & cc!='-99')
  return(population)
}

#' Coordinates Map
#'
#' @param coordinates_dataset
#'
#' @return
#' @export
#'
#' @examples
coordinatesMap <- function(coordinates_dataset) {
  coordinates_map <- getMap()
  plot(coordinates_map)
  points(coordinates_dataset$longitude,coordinates_dataset$latitude, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.6),
         pch = 16, cex = 6*(coordinates_dataset$attacks/max(coordinates_dataset$attacks)))
}

#' Countries Map
#'
#' @param countries_dataset
#'
#' @return
#' @export
#'
#' @examples
countriesMap <- function(countries_dataset) {
  mapped_data <- joinCountryData2Map(countries_dataset, joinCode = "ISO2",
                                     nameJoinColumn = "cc")
  mapCountryData(mapped_data, nameColumnToPlot = "attacks",
                 mapTitle = "Mapa de calor de los países atacantes", catMethod = "pretty",
                 colourPalette = "heat")
}

#' Countries Map (attacks/population)
#'
#' @param countries_dataset
#'
#' @return
#' @export
#'
#' @examples
countriesMapNorm <- function(countries_dataset) {
  mapped_data <- joinCountryData2Map(countries_dataset, joinCode = "ISO2",
                                     nameJoinColumn = "cc")
  mapCountryData(mapped_data, nameColumnToPlot = "attacks_population",
                 mapTitle = "Mapa de calor de los países atacantes (normalizado)", catMethod = "pretty",
                 colourPalette = "heat")
}

#' Host attacks
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
host_attacks <- function(dataset) {
  hosts <- data.frame(dataset %>% filter(!is.na(host)))
  hosts <- data.frame(hosts %>% group_by(host) %>% summarize(attacks=n()))
  return(hosts)
}
