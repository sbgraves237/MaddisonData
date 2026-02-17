#' Two-dimensional growth model for `KFAS`
#'
#' `growthModel` returns a list returned by [KFAS::SSMcustom()] for a model 
#' with potentially irregularly spaced univariate observations with a
#' 2-dimensional (`level`, `growthRate`) state. 
#' 
#' @param sigma a numeric vector, forced to length 2 by replacing it by 
#' `rep(sigma, length=2)`. The one-step transition variance for 
#' `level` is `Q[1,1] = sigma[1]^2` and for `growthRate` is 
#' `Q[2,2] = sigma[2]^2`. With `m` missing values, 
#' `Q[1,1] = (m+1)*sigma[1]^2`, `Q[2,2] = (m+1)*sigma[2]^2`, and 
#' `Q[1,2] = Q[2,1] = sqrt((m+1)*choose(m+1,2)*sigma[1]*sigma[2]`. 
#' @param y = optional numeric vector or `ts` object of length at least 2 used 
#' to estimate the `a1` parameter for [KFAS::SSMcustom()] if `a1` is not 
#' supplied and for `Time` if it is not supplied. 
#' `a1 <- (if(Log) log(c(y[1], y[2]/y[1])) else c(y[1], y[2]-y[1]))`. 
#' @param a1 = optional numeric vector of length 2 to pass to 
#' `KFAS::SSMcustom`. If supplied, the numeric values of `y` are ignored. 
#' @param Time = optional integer vector of times at which non-missing 
#' observations are available. Default = `time(y)` if `y` is of class `ts` or 
#' or `names(y)` if `!is.null` or `1:length(y)` otherwise. 
#' @param stateNames = `c('level', 'growthRate')`
#' @param Log default = TRUE. 
#' @param \dots optional arguments passed to [KFAS::SSMcustom()].
#' 
#' @returns a list returned by [KFAS::SSMcustom()]. 
#' 
#' @export
#'
#' @examples
#' GBR <- subset(MaddisonData, (ISO=='GBR') & !is.na(gdppc))
#' growthMdl1 <- growthModel(.1, GBR$gdppc, Time=GBR$year)
#' GBRgdppc1 <- with(GBR[-1, ], ts(gdppc, year[1]))
#' growthMdl2 <- growthModel(c(.1, .2), GBRgdppc1)
#' growthMdl0 <- growthModel(.1, GBR$gdppc, a1=c(10, 1), Log=FALSE, 
#'                   stateNames=c('lvl', 'vel'))
#' 
#' @keywords models
growthModel <- function(sigma, y, a1, Time, 
                  stateNames = c('level', 'growthRate'), Log= TRUE, ...){
##
## 1. Force sigma to length 2
##  
  Sig <- rep(sigma, length=2)
##
## 2. a1   
##
  if(missing(a1)){
    a1 <- (if(Log) log(c(y[1], y[2]/y[1])) else c(y[1], y[2]-y[1]))
  } else {
    if(length(a1) != 2){
      cat('head(a1) = ', utils::head(a1), '\n')
      stop('a1 supplied; must be of length = 2; length(a1) = ', length(a1))
    }
  }
##
## 3. Time 
  if(missing(Time)){
    if(stats::is.ts(y)) {
      Time <- stats::time(y)
    } else {
    if(!is.null(dim(y)))
      stop('y should be univariate; dim(y) = ', paste(dim(y), collapse=', '))
    }
    Time <- names(y)
    if(is.null(Time)) Time <- 1:length(y)
  } else {
    if(length(Time) != length(y)){
      cat('head(Time) =', utils::head(Time), '\n; head(y) = ', utils::head(y), 
          '\n')
      lenMsg <- 'length(Time) should equal length(y)'
      stop(lenMsg, '; length(Time) = ', length(Time), 
           '; length(y) = ', length(y))
    }
  }
## 
## 4. Observation matrix Z in y = Z a + e  
##  
  Z <- matrix(1:0, 1, 2)
# SSMcustom strips off the dimnames. Included for debugging
  dimnames(Z) <- list('yname', stateNames)
##
## 5. Transition matrices T and R in a = T a + R eta  
##  
  nobs <- length(Time)
  T2 <- array(1, c(2, 2, nobs))
  T2[2,1,] <- 0
  dYr <- diff(Time)
  dY <- c(dYr[1], dYr)
  T2[1,2,] <- dY 
  dimnames(T2) <- list(stateNames, stateNames, Time)
  T2_ <- T2[, , nobs, drop=TRUE]
#  
  R2 <- array(1, c(2, 2, nobs))
  R2[2, 1, ] <- 0 
  R2[1, 2, ] <- dY 
  dimnames(R2) <- list(stateNames, paste0(stateNames, 'Eta'))
  R2_ <- R2[,,nobs, drop=TRUE]
##
## 6. Q2 = var(eta)
##   
  Q2 <- array(0, c(2, 2, nobs) )
  # FOR IRREGULAR TIME SERIES:   
  # Q2 = diag(Sig[1], 0) + sum(i=0:(dY-1), tcrossprod(T^i))*Sig[2]
  # where T = matrix(c(1, 0, 1, 1), 2) = 0 in lower left and 1 o.w. 
  # so T^i = matrix(c(1, 0, i, 1), 2) = 0 in lower left, 1 diag ... 
  # and tcrossprod(T^i) = matrix(c(1+i^2, i, i, 1), 2)
  # It can be shown by induction that 
  # sum(i=0:(dY-1), tcrossprod(T^i)) = 
  # matrix(c(dY*(dY-1)*(2*dY-1)/6, choose(dY, 2), choose(dY, 2), 1), 2)
# 
  Q2[1,1,] <- (dY*Sig[1]^2 + (dY*(dY-1)*(2*dY-1)/6)*Sig[2])
  Q2[2,2,] <- dY*Sig[2]^2
  Q12 <- choose(dY, 2)*Sig[2]
  Q2[1,2,] <- Q12
  Q2[2,1,] <- Q12
  dimnames(Q2) <- list(stateNames, stateNames, Time)
  Q2_ <- Q2[,,nobs, drop=TRUE]
##
## 7. call SSMcustom  
##  
  SSMc <- KFAS::SSMcustom(Z=Z, T=T2, R=R2, Q=Q2, a1=a1, n = nobs, 
                    state_names = stateNames)
  SSMc
}