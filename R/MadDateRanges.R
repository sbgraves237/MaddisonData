#' Convert a vector of date ranges into a data.frame 
#'
#' `MadDateRanges` returns a [`data.frame`] with 3 numeric columns: 
#' `yearBegin`, `yearEnd`, and `sourceNum` from the vector of `dateRanges` 
#' associated with different sources in [`MaddisonSources`]. 
#' 
#' @param dateRanges character vector of date ranges, each associated with a 
#' different source. 
#' 
#' @returns a `data.frame` with 3 columns 
#' \describe{
#'   \item{yearBegin, yearEnd}{numeric years}
#'   \item{sourceNum}{1, 2, 3, ... for the location in `dateRanges`}
#' }
#' 
#' @export
#'
#' @examples
#' MadDateRanges(c('1', '700 – 1500', '1252–1700 (England)', 
#'       '1915-1919 & 1949', '1820, 1870, 1913, 1950'))
#' # equal 
#' data.frame(
#' yearBegin=c(1,  700, 1252, 1820, 1870, 1913, 1950), 
#' yearEnd  =c(1, 1500, 1700, 1820, 1870, 1913, 1950), 
#' sourceNum=c(1, 2, 3, rep(4, 4)))
#' 
#' @keywords manip 
MadDateRanges <- function(dateRanges){
  nSources <- length(dateRanges)
  dateRng0 <- strsplit(dateRanges, "[\\(]")
  dateRng1 <- sapply(dateRng0, '[', 1)
  dateRngs <- strsplit(dateRng1, "[,&]")
  dtRngs <- data.frame(yearBegin=integer(0), yearEnd=integer(0), 
                       sourceNum=integer(0))
  for(i in 1:nSources){
    Rngsi <- strsplit(dateRngs[[i]], "[-\U2013]")
    nR <- length(Rngsi)
    for(iR in 1:nR){
      if(length(Rngsi[[iR]])>2){
        cat('ERROR: source ', i, ' in\n')
        print(dateRanges)
        stop('gives a range with more than 2 years')
      }
      yrs <- rep(as.integer(Rngsi[[iR]]), length=2)
      dtRngs <- rbind(dtRngs, 
        data.frame(yearBegin=yrs[1], yearEnd=yrs[2], 
                   sourceNum=i) 
        ) 
    }
  }
  dtRngs
}