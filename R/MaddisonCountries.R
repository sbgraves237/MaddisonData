#' Maddison Project data 
#'
#' @description
#' The 
#' \href{https://en.wikipedia.org/wiki/Maddison_Project}{Maddison project} 
#' collates historical economic statistics from many sources. 
#' `MaddisonCountries` is a [`data.frame`] of all (`countrycode`, `country`, 
#' `region`) combinations in those data. 
#'
#' @format ## `MaddisonCountries`
#' A data frame with 3 columns: 
#' \describe{
#'   \item{ISO}{3-letter ISO country code}
#'   \item{country}{Country name used by the Maddison project}
#'   \item{region}{Geographic region including `country`}
#' }
#' 
#' Its `rownames` = `ISO`. 
#' 
#' @examples
#' # Get the country for a countrycode (IS)
#' subset(MaddisonCountries, ISO=='GBR', country)
#' # Or
#' MaddisonCountries['GBR', 'country']
#' # Find Yugoslavia 
#' subset(MaddisonCountries, grepl('Yugo', country), 1:3)
#' # number of countries by region 
#' table(MaddisonCountries$region)
#' # What are "Western Offshoots"? 
#' subset(MaddisonCountries, grepl('Of', region), c(country, ISO))
#' @source <https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2020?lang=en>"Groningen Growth and Development Centre"
"MaddisonCountries"
