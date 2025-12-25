#' Identify leading countries
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
#' @returns an object of class `c('MaddisonLeaders', 'data.frame')`, with 
#' columns 
#' 
#' \itemize{ 
#' \item `paste0(x, 'Begin)`, 
#' \item `paste0(x, 'End')`, 
#' \item `paste0(y, '0')`, 
#' \item `paste0(y, '1')`, and 
#' \item `{{group}}`  
#' \item `dy0 = paste0(x, 'End') - paste0(x, 'Begin') + 1` and 
#' \item {
#'   `dy1 = c(tail(paste0(x, 'Begin'), -1) - head(paste0(x, 'End'), -1), NA)` 
#'     (defaults: 
#'     `dy0 = yearEnd - yearBegin +1` and 
#'     `dy1 = c(tail(yearBegin, -1) - head(yearEnd, -1), NA)`
#'     ) 
#'   }
#' }
#' 
#' (defaults: 
#' \itemize{
#' \item `yearBegin`, 
#' \item `yearEnd`, 
#' \item `gdppc0`, 
#' \item `gdppc1`, and 
#' \item `ISO`, plus 
#' \item `dy0 = `yearEnd - yearBegin + 1` and 
#' \item `dy1 = c(tail(yearBegin, -1) - head(yearEnd, -1), NA)` 
#' }
#' 
#' with an attribute `LeaderByYear` = a `data.frame` with columns, `{{x}}`, 
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
MaddisonLeaders <- function(except=character(0), y='gdppc', group='ISO', 
                            data=MaddisonData::MaddisonData, x='year'){
##
## 1. compute LeaderByYear [named "Leaders" for historical reasons]
##   
  yNA <- is.na(data[, y])
  Data <- data[!yNA, ]
  years <- table(Data[, x])
  nYrs <- length(years)
  Leaders <- data.frame(year=as.integer(names(years)), 
                        maxGDPpc=rep(0L, nYrs), ISO=rep('', nYrs))
  rownames(Leaders) <- names(years)
  Yrs <- as.integer(names(years))
  for(i in 1:nYrs){
    Dati <- Data[Data[, x]==Yrs[i], ]
    jmax <- which.max(Dati[, y, drop=TRUE])
    Leaders[i, 'maxGDPpc'] <- Dati[jmax, y, drop=TRUE]
#   find group(s)    
    jma_ <- which(Dati[, y, drop=TRUE]==Dati[jmax, y, drop=TRUE])
    jma <- paste(sort(Dati[jma_, group, drop=TRUE]), collapse=':')
    Leaders[i, 'ISO'] <- jma 
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
#
  LeaderSum$dy0 <- with(LeaderSum, yearEnd- yearBegin+1)
  LeaderSum$dy1 <- with(LeaderSum, c(tail(yearBegin, -1) - 
                                     head(yearEnd, -1), NA)) 
  names(LeaderSum)[1:5] <- c(paste0(x, c('Begin', 'End')), paste0(y, 0:1), 
                             group) 
  attr(LeaderSum, 'LeaderByYear') <- Leaders
  class(LeaderSum) <- c('MaddisonLeaders', 'data.frame')
  LeaderSum
}
