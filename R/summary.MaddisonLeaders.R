#' Summary method for an object of class `MaddisonLeaders`
#'
#' `summary.MaddisonLeaders` returns a [`data.frame`] with columns `ISO`, 
#' `paste0(x, 'Begin)`, `paste0(x, 'End')`, `n`, and `p`. 
#' 
#' @param object = object of class `MaddisonLeaders`. 
#' @param ... = optional arguments for `summary` (not used)
#' 
#' @returns a [`data.frame`] with columns 
#' 
#' \itemize{ 
#' \item `ISO` = One row for each level of `ISO` in `unique(object[, 'ISO'])`
#' \item `paste0(x, 'Begin)` = earliest `object[, paste0(x, 'Begin')]` for `ISO`
#' \item `paste0(x, 'End')`, last `object[, paste0(x, 'End')]` for `ISO`
#' \item `n` = sum of `(paste0(x, 'End')` - `paste0(x, 'Begin') + 1` for `ISO`. 
#' \item `p` = `n/(paste0(x, 'End') - paste0(x, 'Begin') + 1)`.           
#' })
#' 
#' (defaults: 
#' \itemize{
#' \item `ISO` = One row for each level of `ISO` in `unique(object[, 'ISO'])`
#' \item `yearBegin` = earliest `object[, 'yearBegin')]` for `ISO`
#' \item `yearEnd` = last `object[, 'yearEnd')]` for `ISO`
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
summary.MaddisonLeaders <- function(object, ...){
##
## 1. ISOs
##   
  ISOs <- sort(unique(object[, 'ISO']))
  sumLdrs <- data.frame(ISO=ISOs, yB=NA, yE=NA, n=NA, p=NA)
##
## 2. find yBegin, yEnd
##  
  iB <- grep('Begin$', names(object))
  nB <- length(iB)
  if(nB<1){
    stop('yBegin not found in names(object) = ', 
         paste(names(object), collapse=', '))
  }
  if(nB>1){
    stop('yBegin found multiple times in names(object) = ', 
        paste(names(object), collapse=', '))
  }
  names(sumLdrs)[2] <- names(object)[iB]
#  
  iE <- grep('End$', names(object))
  nE <- length(iE)
  if(nE<1){
    stop('yEnd not found in names(object) = ', 
         paste(names(object), collapse=', '))
  }
  if(nE>1){
    stop('yEnd found multiple times in names(object) = ', 
        paste(names(object), collapse=', '))
  }
  names(sumLdrs)[3] <- names(object)[iE]
##
## 3. for(iSO in ISO)
##  
  rownames(sumLdrs) <- ISOs
  for(iSO in ISOs){
    sel <- (object[, 'ISO'] == iSO) 
    sumLdrs[iSO, 2] <- min(object[sel, iB])
    sumLdrs[iSO, 3] <- max(object[sel, iE])
    sumLdrs[iSO, 'n'] <- sum(object[sel, iE]-object[sel, iB]+1)
    sumLdrs[iSO, 'p'] <- (sumLdrs[iSO, 'n'] / 
                (sumLdrs[iSO, 3] - sumLdrs[iSO, 2] + 1) )
  }
##
## 3. Done
##
  sumLdrs
}
