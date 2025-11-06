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
#' @returns an [`data.frame`] with columns `paste0(x, 'Begin)`, 
#' `paste0(x, 'End')`, `paste0(y, '0')`, `paste0(y, '1')`, and `{{group}}` 
#' (defaults: `yearBegin`, `yearEnd`, `gdppc0`, `gdppc1`, and `ISO`) with an 
#' attribute `LeaderByYear` = a `data.frame` with columns, `{{x}}`, 
#' `paste0('max', y)`, and `{{group}}` (defaults: `year`, `maxgdppc`, `ISO`). 
#' 
#' @export
#'
#' @examples
#' Leaders0 <- MaddisonLeaders() # max GDPpc for each year. 
#' 
#' # Presumed technology leaders without commodity leaders with narrow 
#' # economies 
#' Leaders1 <- MaddisonLeaders(c('ARE', 'KWT', 'QAT')) 
#' # since 1600 
#' MadDat1600 <- subset(MaddisonData, year>1600)
#' Leaders1600 <- MaddisonLeaders(c('ARE', 'KWT', 'QAT'), data=MadDat1600)
#' 
#' @keywords manip 
MaddisonLeaders <- function(except=character(0), y='gdppc', 
                            group='ISO', data=MaddisonData, x='year'){
##
## 1. compute LeaderByYear [named "Leaders" for historical reasons]
##   
  years <- table(data[, x])
  nYrs <- length(years)
  Leaders <- data.frame(year=as.integer(names(years)), 
                        maxGDPpc=rep(0L, nYrs), ISO=rep('', nYrs))
  rownames(Leaders) <- names(years)
  ctries <- with(MaddisonCountries, ISO[!(ISO %in% except)])
  nCtries <- length(ctries)
  for(i in 1:nCtries){
    seli0 <- which(data[, group] == ctries[i])
    dati <- data[seli0, ]
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
                          gdppc0 = Leaders[1, 'maxGDPpc'], 
                          gdppc1 = Leaders[1, 'maxGDPpc'], 
                          ISO = Leaders[1, 'ISO'])
  for(j in 2:nYrs){
    if(Leaders[j, 'ISO']==Leaders[j-1, 'ISO']){
      LeaderSum[i, 'yearEnd'] <- Leaders[j, 'year']
      LeaderSum[i, 'gdppc1'] <- Leaders[j, 'maxGDPpc']
    } else {
      i <- i+1 
      LeaderSum[i, c('yearBegin', 'yearEnd')] <- Leaders[j, 'year']
      LeaderSum[i, c('gdppc0', 'gdppc1')] <- Leaders[j, 'maxGDPpc']
      LeaderSum[i, 'ISO'] <- Leaders[j, 'ISO']
    }
  }
##
## 3. Done
##
  names(Leaders) <- c(x, paste0('max', y), group)
  names(LeaderSum) <- c(paste0(x, c('Begin', 'End')), paste0(y, 0:1), group) 
  attr(LeaderSum, 'LeaderByYear') <- Leaders
  LeaderSum
}
