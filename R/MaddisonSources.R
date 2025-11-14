#' Maddison Project data 
#'
#' @description
#' The 
#' \href{https://en.wikipedia.org/wiki/Maddison_Project}{Maddison project} 
#' collates historical economic statistics from many sources. 
#' `MaddisonSources` is a [`list`] of [`tibble::tibble`]s with `ISO` names 
#' giving the sources of `GDP` per capita for different years for the said 
#' country. 
#' 
#' `MaddisonYears` is a [`data.frame`] giving `yearBegin` and `yearEnd` and the 
#' number of each source in `MaddisonSpources` for each `ISO`.
#'
#' @format ## `MaddisonSources`
#' A named list of [`tibble::tibble`]s, one for each country, named with the 
#' ISO country codes. Each tibble has one row for each source for the indicated 
#' ISO and two columns: 
#' \describe{
#'   \item{years}{
#'      character variable of year(s) for this source starting with year 1 CE. 
#'    }
#'   \item{source}{character variable giving the source for the `years` 
#'      described.
#'   }
#' }
#' In addition, `MaddisonSources` has an attribute `since2008`, which says, 
#' "`gdppc` since 2008: Total Economy Database (TED) from the Conference Board 
#' for all countries included in TED and UN national accounts statistics for 
#' all others."
#' 
#' @format ## `MaddisonYears`
#' A [`data.frame`]s with 4 columns: 
#' \describe{
#'   \item{ISO}{3-letter country code. }
#'   \item{yearBegin, yearEnd}{
#'      Integer year begin and end for each source.
#'    }
#'   \item{sourceNum}{
#'      Integer of the source within `MaddisonSources[[ISO]]`. 
#'   }
#' }
#'
#' @examples
#' MaddisonSources[['GBR']]
#' MaddisonSources[['GBR']][, 1, drop=TRUE] 
#' # = c('1', '1252–1700 (England)', '1700–1870') 
#' # for data from the year 1 
#' # and for England only between 1252 and 1700, etc. 
#' 
#' MaddisonSources[['IRN']][, 1, drop=TRUE] 
#' # = '1820, 1870, 1913, 1950'
#' # for those 4 years only. 
#' 
#' MaddisonSources[c('GBR', 'USA')]
#' 
#' MaddisonSources[['GBR']][, 1, drop=TRUE] 
#' # = c('1', '1252–1700 (England)', '1700–1870') 
#' 
#' MaddisonYears[MaddisonYears$ISO=='GBR', ] = 
#' data.frame(
#' ISO=rep('GBR', 3), 
#' yearBegin=c(1, 1252, 1700), 
#' yearEnd  =c(1, 1700, 1870), 
#' sourceNum=1:3
#' )
#' 
#' MaddisonSources[['EGY']][, 1, drop=TRUE] 
#' # = c('1', '700 – 1500', '1820, 1870, 1913, 1950')
#' 
#' MaddisonYears[MaddisonYears$ISO=='EGY', ] = 
#' data.frame(
#' ISO=rep('EGY', 6), 
#' yearBegin=c(1,  700, 1820, 1870, 1913, 1950), 
#' yearEnd  =c(1, 1500, 1820, 1870, 1913, 1950), 
#' sourceNum=c(1,    2, rep(3, 4))
#' )
#' 
#' @source <https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2020?lang=en>"Groningen Growth and Development Centre"
"MaddisonSources"

#' @rdname MaddisonSources
"MaddisonYears"


