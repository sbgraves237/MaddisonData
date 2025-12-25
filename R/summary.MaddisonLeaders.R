#' Summary method for an object of class `MaddisonLeaders`
#'
#' `summary.MaddisonLeaders` returns a [`data.frame`] with columns `ISO`, 
#' `paste0(x, 'Begin)`, `paste0(x, 'End')`, `n`, and `p`. 
#' 
#' @param data = object of class `MaddisonLeaders`. 
#' 
#' @returns a [`data.frame`] with columns 
#' 
#' \itemize{ 
#' \item `ISO` = One row for each level of `ISO` in `unique(data[, 'ISO'])`
#' \item `paste0(x, 'Begin)` = earliest `data[, paste0(x, 'Begin')]` for `ISO`
#' \item `paste0(x, 'End')`, last `data[, paste0(x, 'End')]` for `ISO`
#' \item `n` = sum of `(paste0(x, 'End')` - `paste0(x, 'Begin') + 1` for `ISO`. 
#' \item `p` = `n/(paste0(x, 'End') - paste0(x, 'Begin') + 1)`.           
#' })
#' 
#' (defaults: 
#' \itemize{
#' \item `ISO` = One row for each level of `ISO` in `unique(data[, 'ISO'])`
#' \item `yearBegin` = earliest `data[, 'yearBegin')]` for `ISO`
#' \item `yearEnd` = last `data[, 'yearEnd')]` for `ISO`
#' \item `n` = sum of `('yearEnd' - 'yearBegin' + 1)` for `ISO`. 
#' \item `p` = `n/(yearEnd - yearBegin + 1)`.            
#' }
#' 
#' @export
#'
#' @examples
#' Leaders0 <- MaddisonLeaders() # max GDPpc for each year. 
#' summary(Leaders0) 
#' 
#' @keywords manip 
summary.MaddisonLeaders <- function(data, ...){
##
## 1. ISOs
##   
  ISOs <- sort(unique(data[, 'ISO']))
  sumLdrs <- data.frame(ISO=ISOs, yB=NA, yE=NA, n=NA, p=NA)
##
## 2. find yBegin, yEnd
##  
  iB <- grep('Begin$', names(data))
  nB <- length(iB)
  if(nB<1){
    stop('yBegin not found in names(data) = ', 
         paste(names(data), collapse=', '))
  }
  if(nB>1){
    stop('yBegin found multiple times in names(data) = ', 
        paste(names(data), collapse=', '))
  }
  names(sumLdrs)[2] <- names(data)[iB]
#  
  iE <- grep('End$', names(data))
  nE <- length(iE)
  if(nE<1){
    stop('yEnd not found in names(data) = ', 
         paste(names(data), collapse=', '))
  }
  if(nE>1){
    stop('yEnd found multiple times in names(data) = ', 
        paste(names(data), collapse=', '))
  }
  names(sumLdrs)[3] <- names(data)[iE]
##
## 3. for(iSO in ISO)
##  
  rownames(sumLdrs) <- ISOs
  for(iSO in ISOs){
    sel <- (data[, 'ISO'] == iSO) 
    sumLdrs[iSO, 2] <- min(data[sel, iB])
    sumLdrs[iSO, 3] <- max(data[sel, iE])
    sumLdrs[iSO, 'n'] <- sum(data[sel, iE]-data[sel, iB]+1)
    sumLdrs[iSO, 'p'] <- (sumLdrs[iSO, 'n'] / 
                (sumLdrs[iSO, 3] - sumLdrs[iSO, 2] + 1) )
  }
##
## 3. Done
##
  sumLdrs
}
