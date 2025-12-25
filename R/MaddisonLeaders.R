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
#' \item {`paste0('d', x, '0') = `
#'      `paste0(x, 'End') - paste0(x, 'Begin') + min(dx)`, where 
#'      `dx = min(diff(sort(unique(data[, x]))))`
#' }
#' \item {
#'   `paste0('d', x, '1') = `
#'   `c(tail(paste0(x, 'Begin'), -1) - head(paste0(x, 'End'), -1), NA)` 
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
#' \item `dyear0 = `yearEnd - yearBegin + 1` and 
#' \item `dyear1 = c(tail(yearBegin, -1) - head(yearEnd, -1), NA)` 
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
#' 
#' # since 1600 
#' MadDat1600 <- subset(MaddisonData, year>1600)
#' Leaders1600 <- MaddisonLeaders(c('ARE', 'KWT', 'QAT'), data=MadDat1600)
#' 
#' # max pop by region within percentiles of gdppc
#' noGDP <- is.na(MaddisonData$gdppc)
#' MadDat <-MaddisonData[!noGDP, ]
#' gdpPcts <- quantile(MadDat$gdppc, seq(0, 1, .01), na.rm=TRUE)
#' gdpPct <- unique(as.numeric(gdpPcts[-1]))
#' gdpPc <-c(gdpPct[-100], tail(gdpPct, 1)*(1+sqrt(.Machine$double.eps)))
#' gdp100 <- MadDat$gdppc
#' nObs <- nrow(MadDat)
#' for(i in 1:nObs){gdp100[i] <- min(gdpPc[MadDat$gdppc[i]<gdpPc])}
#' MadDat$gdp100 <- gdp100
#' MadDat$region <- MaddisonCountries[MadDat$ISO, 'region', drop=TRUE]
#' MadPopRgnGDP<-MaddisonLeaders(y='pop',group='region',data=MadDat,x='gdp100')
#' 
#' @param data [`data.frame`] or [`tibble::tibble`] with first two columns 
#' being `ISO` and `year` and `y` being the name of another column. 
#' @param x time variable. Default = `year`. 
#' 
#' @keywords manip 
MaddisonLeaders <- function(except=character(0), y='gdppc', group='ISO', 
                            data=MaddisonData::MaddisonData, x='year'){
##
## 1. compute LeaderByYear [named "Leaders" for historical reasons]
##   
  yNA <- is.na(data[, y])
  Data <- data[!yNA, ]
  Xt <- table(Data[, x])
  X_ <- names(Xt)
  Xn <- as.numeric(X_)
  nX <- length(Xn)
# Xn may not match any in data[, x]
  nobs <- nrow(Data)
  X <- rep(NA, nobs)
  for(i in 1:nobs){
    dxi <- abs(Xn-Data[i, x, drop=TRUE])
    X[i] <- Xn[which.min(dxi)]
  }
  Leaders <- data.frame(x=Xn, maxY=rep(0, nX), Gp=rep('', nX))
  rownames(Leaders) <- X_
  for(i in 1:nX){
    Dati <- Data[X==Xn[i], ] 
    jmax <- which.max(Dati[, y, drop=TRUE])
    Leaders[i, 'maxY'] <- Dati[jmax, y, drop=TRUE]
#   find group(s)    
    jma_ <- which(Dati[, y, drop=TRUE]==Dati[jmax, y, drop=TRUE])
    jma <- paste(sort(Dati[jma_, group, drop=TRUE]), collapse=';')
    Leaders[i, 'Gp'] <- jma 
  }
##
## 2. Leaders [named "LeadersSum", because LeaderByYear was named "Leaders"]
##
  i <- 1
  LeaderSum <- data.frame(xBegin=Leaders[1, 'x'], 
                          xEnd  =Leaders[1, 'x'], 
                          y0 = Leaders[1, 'maxY'], 
                          y1 = Leaders[1, 'maxY'], 
                          Gp = Leaders[1, 'Gp'])
  for(j in 2:nX){
    if(Leaders[j, 'Gp']==Leaders[j-1, 'Gp']){
      LeaderSum[i, 'xEnd'] <- Leaders[j, 'x']
      LeaderSum[i, 'y1'] <- Leaders[j, 'maxY']
    } else {
      i <- i+1 
      LeaderSum[i, c('xBegin', 'xEnd')] <- Leaders[j, 'x']
      LeaderSum[i, c('y0', 'y1')] <- Leaders[j, 'maxY']
      LeaderSum[i, 'Gp'] <- Leaders[j, 'Gp']
    }
  }
##
## 3. Done
##
  names(Leaders) <- c(x, paste0('max', y), group)
#
  dx0 <- min(diff(Xn))
  LeaderSum$dx0 <- with(LeaderSum, xEnd-xBegin+dx0)
  LeaderSum$dx1 <- with(LeaderSum, c(tail(xBegin, -1) - 
                                     head(xEnd, -1), NA)) 
  names(LeaderSum) <- c(paste0(x, c('Begin', 'End')), paste0(y, 0:1), 
                             group, paste0('d', x, 0:1)) 
  attr(LeaderSum, 'LeaderByYear') <- Leaders
  class(LeaderSum) <- c('MaddisonLeaders', 'data.frame')
  LeaderSum
}
