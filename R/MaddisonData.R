#' Maddison Project data 
#'
#' @description
#' The 
#' \href{https://en.wikipedia.org/wiki/Maddison_Project}{Maddison project} 
#' collates historical economic statistics from many sources. 
#' `MaddisonCountries` is a [`data.frame`] of all (`countrycode`, `country`, 
#' `region`) combinations in those data. This object provides easy access to 
#' the 2023 version of the Maddison project data downloaded 2025-08-28. 
#'
#' @format ## `MaddisonData`
#' A data frame with 4 columns: 
#' \describe{
#'   \item{ISO}{3-letter ISO country code}
#'   \item{year}{numeric year starting with year 1 CE}
#'   \item{gdppc}{Gross domestic product (GDP) per capita in 2011 dollars at 
#'                purchasing power parity (PPP)}
#'   \item{pop}{Population, mid-year (thousands)}
#' }
#' @examples
#' # Get the countrycode for a country
#' subset(MaddisonCountries, country=='United Kingdom', ISO)
#' # Select  
#' str(GBR <- MaddisonData[MaddisonData$ISO=='GBR', ])
#' @source <https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2020?lang=en>"Groningen Growth and Development Centre"
"MaddisonData"
