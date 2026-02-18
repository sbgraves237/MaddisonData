#' Update function to estimating a two-dimensional growth model
#'
#' `growthUpdateFn` 
#' 
#' returns a list returned by [KFAS::SSMcustom()] for a model 
#' with potentially irregularly spaced univariate observations with a
#' 2-dimensional (`level`, `growthRate`) state. 
#' 
#' @param pars = `log(variance)` of noise for `[1] observations`, 
#' `[2] level`, and `[3] growthRate`. Forced to length 3 via 
#' `c(pars, rep(tail(pars, 1), length=3-length(pars)))`. With no missing 
#' values, the one-step transition variance for `level` is 
#' `Q[1,1] = exp(pars[2]` and for `growthRate` is `Q[2,2, ] = exp(pars[3])`. 
#' With `m` missing values, `Q[1,1, ] = (m+1)*exp(pars[2])`, 
#' `Q[2,2] = (m+1)*exp(pars[3])`, and 
#' `Q[1,2, ] = Q[2,1] = sqrt((m+1)*choose(m+1,2)*exp(mean(pars[2:3]))`. 
#' @param model = list assumed to have components `n` = third dimension of the 
#' transition covariance array `Q` and 
#' `state_names = c('level', 'growthRate')` as returned by [`growthModel`]. 
#' @param Time = optional integer vector of times at which non-missing 
#' observations are available. Default = `time(y)` if `model` has a component 
#' `y` `names(y)` if `!is.null` or `1:n` otherwise. 
#' 
#' @returns a `model` with components `H` and `Q` updated as described. 
#' 
#' @export
#'
#' @examples
#' GBR <- subset(MaddisonData, (ISO=='GBR') & !is.na(gdppc))
#' growthMdl1 <- growthModel(.1, GBR$gdppc, Time=GBR$year)
#' growthMdl1v0 <- growthUpdateFn(.1, growthMdl1)
#' growthMdl1v <- growthUpdateFn(.1, growthMdl1, Time=GBR$year)
#' growthMdl1v3 <- growthUpdateFn(1:3, growthMdl1, Time=GBR$year)
#' 
#' growthFml <- (log(gdppc)~ -1 + SSMbespoke(growthModel(.1, GBR$gdppc) )) 
#' library(KFAS)
#' growthSSmdl <-SSModel(growthFml, GBR, H=matrix(NA) )
#' growthSSmdl1 <- growthUpdateFn(.1, growthSSmdl)
#' 
#' @keywords models
growthUpdateFn <- function(pars, model, Time){
##
## 1. pars 
##
  kp <- length(pars)
  if(kp>3){
    stop('pars must have length at most 3; is ', kp)
  }
  if(!is.numeric(pars)){
    stop('pars must be numeric; class(pars) = ', 
         paste(class(pars), collapse=', '))
  }
  Pars <- c(pars, rep(utils::tail(pars, 1), length=3-length(pars)))
  v <- exp(Pars)
##
## 2. y and n
##
  y <- model$y 
  if(is.null(y)){
    n <- model$n
    if(is.null(n)){
      stop("model must have component y or n; has neither.")
    } 
    if(length(n)!=1){
      stop('model$n must have length 1; is ', length(n))
    }
    N <- as.integer(n)
    if((N-n != 0) || (n<=0)){
      stop('model$n must be a positive integer; is ', n)
    }
  } else {
    N <- length(y)
  }
##
## 3. Time 
## 
  if(missing(Time)){
    if(is.null(y)){ 
      Tm <- 1:N 
      dT <- rep(1, N)
    } else {
      Tm <- as.numeric(stats::time(y))
#      N <- length(Tm)
      dTm <- diff(Tm)
      dT <- c(dTm[1], dTm)
    }
  } else {
    Tm <- Time 
    NT <- length(Time)
    if(NT != N){
      stop('length(Time) must equal model$n or NROW(model$y); is ', NT)
    }
    if(!is.numeric(Time)){
      stop('Time must be numeric; is ', 
           paste(class(Time), collapse=', '))
    }
    dT_ <- diff(Time)
    dT <- c(dT_[1], dT_)
  }
##
## 4. H
##  
  Model <- model 
  H <- array(v[1], c(1,1,1))# observation variance 
  Model$H <- H
## 
## 5. Q 
##  
  Q2 <- array(0, c(2, 2, N) )
  # FOR IRREGULAR TIME SERIES:   
  # Q2 = diag(v[1], 0) + sum(i=0:(dT-1), tcrossprod(T^i))*v[2]
  # where T = matrix(c(1, 0, 1, 1), 2) = 0 in lower left and 1 o.w. 
  # so T^i = matrix(c(1, 0, i, 1), 2) = 0 in lower left, 1 diag ... 
  # and tcrossprod(T^i) = matrix(c(1+i^2, i, i, 1), 2)
  # It can be shown by induction that 
  # sum(i=0:(dT-1), tcrossprod(T^i)) = 
  # matrix(c(dT*(dT-1)*(2*dT-1)/6, choose(dT, 2), choose(dT, 2), 1), 2)
  # 
  Q2[1,1,] <- (dT*v[2] + (dT*(dT-1)*(2*dT-1)/6)*v[3])
  Q2[2,2,] <- dT*v[3]
  Q12 <- choose(dT, 2)*v[3]
  Q2[1,2,] <- Q12
  Q2[2,1,] <- Q12
#  Q2_ <- Q2[,,n, drop=TRUE]
##
## 6. state_names
## 
  stateN <- model$state_names
  dimnames(Q2) <- list(stateN, stateN, Tm)
  Model$Q <- Q2
  Model
}