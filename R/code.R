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

#' Title
#'
#' @return
#' @export
#'
#' @examples
getSourceData <- function(){
  filepath <- '../dataset/marx-geo.csv'
  dataset <- read.csv(file=filepath, header=TRUE, sep=',', check.names=TRUE,
                      stringsAsFactors = FALSE,
                      na.strings=c(""))
  useful_vars <- c("datetime","country","latitude","longitude")
  useful_dataset <- dataset[useful_vars]

  return(useful_dataset)
}

#' Remove NAs from coordinates
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
removaNACoordinates <- function(dataset) {
  clean_dataset <- data.frame(dataset %>% filter(!is.na(latitude) & !is.na(longitude)))
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
  grouped_dataset <- data.frame(dataset %>% group_by(longitude,latitude) %>% summarize(times=n()))
  return(grouped_dataset)
}

#' Remove over range coordinates
#' @details LAT[-90:90] LONG [-180:180]
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
removeOverRangeCoordinates <- function(dataset) {
  clean_dataset <- data.frame(dataset %>% filter(latitude<=90))
  clean_dataset <- data.frame(clean_dataset %>% filter(latitude>=-90))
  clean_dataset <- data.frame(clean_dataset %>% filter(longitude<=180))
  clean_dataset <- data.frame(clean_dataset %>% filter(longitude>=-180))
  return(clean_dataset)
}

#' Remove NAs from countries
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
removeNACountries <- function(dataset) {
  clean_dataset <- data.frame(dataset %>% filter(!is.na(country)))
  return(clean_dataset)
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
  grouped_countries <- data.frame(dataset %>% group_by(country) %>% summarize(times=n()))
  return(grouped_countries)
}

#' Rewrite some country names
#'
#' @param grouped_countries
#'
#' @return
#' @export
#'
#' @examples
rewriteCountries <- function(grouped_countries) {
  grouped_countries <- grouped_countries %>% mutate(
    country = if_else(country == "Antigua and Barbuda", 'Antigua',
                      if_else(country == "British Virgin Islands", 'Virgin Islands',
                              if_else(country == "Czechia", 'Czech Republic',
                                      if_else(country == "Hong Kong", 'China',
                                              if_else(country == "Macao", 'China',
                                                      if_else(country == "Myanmar [Burma]", 'Myanmar',
                                                              if_else(country == "Saint Vincent and the Grenadines", 'Saint Vincent',
                                                                      if_else(country == "Trinidad and Tobago", 'Trinidad',
                                                                              if_else(country == "United Kingdom", 'UK',
                                                                                      if_else(country == "United States", 'USA', country)
                                                                              ))))))))))
  return(grouped_countries)
}

#' Coordinates Map
#'
#' @param cc2
#'
#' @return
#' @export
#'
#' @examples
coordinatesMap <- function(cc2) {
  world_data <- map_data('world')
  world <- ggplot() + borders("world", colour="#D7D7D7", fill="#CBCBCB") + theme_map()
  map_coordinates <- world +
    geom_point(aes(x = longitude, y = latitude, size = times), data = cc2, colour = '#0000FF', alpha = .25)

}

#' Countries Map
#'
#' @param grouped_countries
#'
#' @return
#' @export
#'
#' @examples
countriesMap <- function(grouped_countries) {
  world_data <- map_data('world')
  world <- ggplot() + borders("world", colour="#D7D7D7", fill="#CBCBCB") + theme_map()
  countries_mid <- mean(grouped_countries$times)
  map_countries <- world +
    geom_map(aes(fill = times, map_id = country),
             map = world_data, data = grouped_countries) +
    scale_fill_gradient2(name = "Times/Country",
                         mid = "#FFFF00", high = "#FF0000") + borders("world", colour="#D7D7D7")
  complete_map <- world +
    geom_map(aes(fill = times, map_id = country),
             map = world_data, data = grouped_countries) +
    scale_fill_gradient2(name = "Times/Country", low = "blue", mid = "yellow",
                         high = "red") +
    geom_point(aes(x = longitude, y = latitude, size = times), data = cc2, colour = 'blue', alpha = .25)


}
