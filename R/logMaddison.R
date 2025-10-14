#' Select countries and add logged variables  
#'
#' `logMaddison` returns a [`tibble::tibble`] of data on selected countries 
#' extracted from `MaddisonData`, appending columns `lnGDPpc` and `lnPop` = 
#' natural logarithms of `gdppc` and `pop`. 
#' 
#' @param ISO either NULL to select all the data in `MaddisonData` or a 
#' character vector of `ISO` codes used in the Maddison project. 
#' 
#' @returns a [`tibble::tibble`] with 6 columns:
#' \describe{
#'   \item{ISO}{3-letter ISO code for countries selected}
#'   \item{year}{numeric year in the current era.}
#'   \item{gdppc}{
#'      Gross domestic product per capita adjusted for inflation to 2011 
#'      dollars at purchasing power parity.
#'   }
#'   \item{pop}{Population, mid-year (thousands)}
#'   \item{lnGDPpc}{log(gdppc)}
#'   \item{lnPop}{log(pop)}
#' }
#' 
#' @export
#'
#' @examples
#' logMaddison() # all 
#' logMaddison(c('GBR', 'USA')) # GBR, USA
#' 
#' @keywords manip 
logMaddison <- function(ISO=NULL){
##
## 1. check ISO
## 
  if(is.null(ISO) || (length(ISO)<1)){
    logMad <- MaddisonData::MaddisonData
  } else {
##
## 2. check ISO 
## 
    testthat::expect_true(all(ISO %in% MaddisonData::MaddisonCountries$ISO))
    logMad <- MaddisonData::MaddisonData[
      MaddisonData::MaddisonData$ISO %in% ISO, ]
  }
##
## 3. add logs
## 
  logMad$lnGDPpc <- log(logMad$gdppc)
  logMad$lnPop <- log(logMad$pop)
##
## 4. Done
## 
  logMad
}