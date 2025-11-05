#' Plot selected countries
#'
#' `MaddisonLeaders` computes the countries with the highest `gdppc` for each 
#' `year`.  
#' 
#' @param except either NULL to select all the data in `MaddisonData` or a 
#' character vector of `group` codes to EXCLUDE, e.g., so the result reflects 
#' apparent  technology leaders, excluding countries whose high `gdppc` may be 
#' due to a dominant position in a single commodity. 
#' @param y name of column in `data` to consider. Default = `gdppc`.  
#' @param group name of column in `data` as the grouping variable. Default = 
#' `ISO`.
#' @param data [`data.frame`] or [`tibble::tibble`] with first two columns 
#' being `ISO` and `year` and `y` being the name of another column. 
#' @param x time variable. Default = `year`. 
#' 
#' @returns an [`data.frame`] with columns `yearBegin`, `yearEnd`, and 
#' `{{group}}` (default for the the third column = `ISO`) with an attribute 
#' `LeaderByYear` = a `data.frame` with columns, `year`, `maxGDPpc`, `ISO`. 
#' 
#' @export
#'
#' @examples
#' Leaders0 <- MaddisonLeaders() # max GDPpc for each year. 
#' 
#' # Presumed technology leaders without commodity leaders with narrow 
#' # economies 
#' Leaders1 <- MaddisonLeaders(c('ARE', 'KWT', 'QAT')) 
#' 
#' @keywords manip 
MaddisonLeaders <- function(except=character(0), y='gdppc', 
                            group='ISO', data=MaddisonData, x='year'){
##
## 1. compute LeaderByYear [named "Leaders" for historical reasons]
##   
  years <- table(MaddisonData[, x])
  nYrs <- length(years)
  Leaders <- data.frame(year=as.integer(names(years)), 
                        maxGDPpc=rep(0L, nYrs), ISO=rep('', nYrs))
  rownames(Leaders) <- names(years)
  ctries <- with(MaddisonCountries, ISO[!(ISO %in% except)])
  nCtries <- length(ctries)
  for(i in 1:nCtries){
    seli0 <- which(MaddisonData[, group] == ctries[i])
    dati <- MaddisonData[seli0, ]
    yrsi <- as.character(dati[, x, drop=TRUE])
#   are there ties?     
    seleq <- which(Leaders[yrsi, 'maxGDPpc']==dati[, y])
    if(length(seleq)>0){
      Leaders[yrsi, 3][seleq] <- 
        paste0(Leaders[yrsi, 3][seleq], ":", dati[seleq, group])
    }
    seli <- which(Leaders[yrsi, 'maxGDPpc'] < dati$gdppc)
    if(length(seli)>0){
      Leaders[yrsi, 2:3][seli, ] <- dati[seli, c(3, 1)]
    }
  }
##
## 2. Leaders [named "LeadersSum", because LeaderByYear was named "Leaders"]
##
  i <- 1
  LeaderSum <- data.frame(yearBegin=Leaders[1, 'year'], 
                           yearEnd =Leaders[1, 'year'], 
                           ISO = Leaders[1, 'ISO'])
  for(j in 2:nYrs){
    if(Leaders[j, 'ISO']==Leaders[j-1, 'ISO']){
      LeaderSum[i, 'yearEnd'] <- Leaders[j, 'year']
    } else {
      i <- i+1 
      LeaderSum[i, c('yearBegin', 'yearEnd')] <- Leaders[j, 'year']
      LeaderSum[i, 'ISO'] <- Leaders[j, 'ISO']
    }
  }
##
## 3. Done
##
  attr(LeaderSum, 'LeaderByYear') <- Leaders
  LeaderSum
}
