#' `ggplot` multiple paths in one panel each with no space between panels and 
#' a shared horizontal axis on the bottom. 
#'
#' `ggplotPath2` accepts a variety of inputs. The most general is with `object`
#' = a list of sublists to be fed individually to `do.call(ggplotPath,..)` and 
#' then assembled into the desired plot after eliminating the space between the 
#' individual plots. Optional `\dots` arguments give default values for the 
#' individual calls to [`ggplotPath`]. 
#' 
#' Alternatively, the first `object` argument can be a matrix, [`data.frame`], 
#' `tibble`, or multivariate time series (of class `mts`). An optional `Time` 
#' argument must be a numeric vector with length equal to the number of rows of 
#' `object`. If `Time` is provided, it overrides any values for the horizontal 
#' axis that could be inferred from [`rownames`] or [`time`] of `object`. A 
#' second optional `object2` can be either a vector whose length matches the 
#' number of rows of `object` or a matrix or `ts` object similarly matching the 
#' number of rows of `object`. 
#' 
#' @param object with an associated matrix to be plotted. The following 
#' classes of objects are supported: 
#'  \itemize{
#'    \item `matrix`, [`data.frame`], `tibble` or `mts` of `dim = c(n, k)`, 
#'      e.g., `k` components of a state vector at `n` points in time. 
#'      `ggplotPath` is called separately for each of the `k` column. Each such 
#'      call returns a `ggplot` object. Those `ggplot` objects are assembled as 
#'      separate panels into a `ggplot` object with a shared horizontal axis 
#'      with no space between the panels with at most one more row than 
#'      `length(Time)` and the first row of `object` is dropped if needed so 
#'      `n == length(k)`.
#'    \item `KFS` (defined in package `KFAS`) calls 
#'      `ggplotPath.matrix(object$a, ...)`. 
#'    \item `list` with a component `a`, assumed to a model produced from  
#'      estimation in the `KFAS` package, calls 
#'      `ggplotPath2(KFAS::KFS(object$a), ...)`.
#'  }
#' @param Time If present, it must be a numeric vector of length equal to the 
#' number of rows of the matrix obtained from `object` and fed to `ggplotPath` 
#' to create the individual panels of the plot. Default depends on 
#' `class(object)`: 
#'  \itemize{
#'    \item `matrix`: Default `Time = rownames(object)`. 
#'    \item `mts`: Default `Time = time(object)`. 
#'    \item `KFS` (defined in package `KFAS`): Default = `rownames` or `time` 
#'      of `object$model$y`. 
#'    \item `model` (defined in package `KFAS`): Default = `rownames` or `time` 
#'      of `object$y`. 
#'  }
#' @param object2 an optional vector or matrix-type object with the same 
#' number of rows as `object` and `k2` columns that provide separate reference 
#' lines to appear in the same panels as the first `k2` columns of `object`. 
#' Obviously, `k2 <= k`. 
#' @param scaley = optional numeric vector of the length `k` fed individually 
#' as the scaley argument accompanying the different successive calls to 
#' `ggplotPath`. If the matrix obtained from `object` has two columns with 
#' names = c('level', 'slope') or c('level', 'growthRate'), then the default 
#' for `scaley` = c(1000, 100). Otherwise, default = 1. 
#' @param logy optional character vector of length `k` to control the scales 
#' used in the individual panels of the plot: 
#'  \itemize{
#'    \item `exp_log`: Feed `exp` of column `i` of the matrix to plot to 
#'    `ggplotPath(..., logy=TRUE)`. 
#'    \item `log`: Feed column `i` of the matrix to plot to 
#'    `ggplotPath(..., logy=TRUE)`. 
#'    \item '': Feed column `i` of the matrix to plot to 
#'    `ggplotPath(..., logy=FALSE)`. 
#'  }
#' The default is 'exp_log' for the first panel and '' for the rest. 
#' @param ylab optional character vector of length `k` for y axis labels. 
#' @param hlines optional list of at most `k` of numeric vectors (possibly with 
#' attributes), for horizontal lines in panel `i`, feeding `hlines[[i]]` to 
#' `ggplotPath(..., hlines = hlines[[i]])` for panel `i = 1:k`. 
#' `color='grey', lty='dotted'` unless `color` or `colour` and / or `lty` 
#' are available as `attr(hlines[[i]], ...)`.  
#' @param vlines optional numeric vector of locations on the `x` axis for 
#' vertical lines using `ggplotPath(..., vlines=vlines)` with `color='grey', 
#' lty='dotted'` unless `color` or `colour` and / or `lty` are available as 
#' `attr(vlines, ...)`.  
#' @param labels = optional [`data.frame`] with columns 
#' `x, y, label, component`, and optionally `srt, col, size`, where `x`, `y`, 
#' `srt`, and `size` are are numeric, `label` is character, and `col` are 
#' acceptable values for `color` in 
#' `with(labels, annotate('text', x=x, y=y, label = label, srt=srt, 
#' color=col, size=size))`. Defaults for `srt`, `col`, and `size`  are 0, 
#' 'black', and 4, respectively. `component` is an integer in `1:k`. 
#' `labelsi = subset(labels, component==i)` is used in 
#' `ggplotPath(..., labels=labelsi)` to label panel `i = 1:k`. 
#' @param fontsize optional number for legend and axes labels, used in 
#' `ggplotPath(..., fontsize=fontsize); default = 10. 
#' @param color optional vector or list of length '`k` of vectors to feed to 
#' `ggplotPath(..., color=color[[i]])` for panel `i=1:k`. If present, 
#' `length(color)` should equal `k`. Default is 2:1 for each panel with a 
#' reference line provided in `object2` and 1 otherwise. See `help(ggplotPath)` 
#' for options. 
#' @param linetype optional vector or list of length `k` of vectors to feed to 
#' `ggplotPath(..., linetype=linetype[[i]])` for panel `i=1:k`. If present, 
#' `length(color)` should equal `k`. Default is 2:1 for each panel with a 
#' reference line provided in `object2` and 1 otherwise. See `help(ggplotPath)` 
#' for options. 
#' @param ... optional arguments.  
#' 
#' @returns an object of class [`ggplot2::ggplot`], which can be subsequently 
#' edited, and whose [`print`] method produces the desired plot. 
#' 
#' @examples
#' # matrix examples 
#' Mat <- cbind(lvl=1:5, vel=rep(1:2, length=5), acc=sin(1:5))
#' Mat1 <- Mat
#' rownames(Mat1) <- 1951:1955
#' Mat2 <- as.data.frame(cbind(Mat, year=1951:1955))
#' 
#' # mts example 
#' MTS <- ts(Mat, 1951)
#' 
#' # Do 
#' Matp <- ggplotPath2(Mat)
#' Mat1p <- ggplotPath2(Mat1)
#' Mat2p <- ggplotPath2(Mat2[, 1:3], Time=Mat2[, 'year']) 
#' MTSp <- ggplotPath2(MTS)
#' MTSep <- ggplotPath2(MTS, logy=c('exp_log', 'log', ''))
#' 
#' # list example 
#' List2 <- list(
#'     level=list('year', 'lvl'), 
#'     slope=list('year', 'vel'), 
#'     accel=list('year', 'acc'))
#' Mat2l <- ggplotPath2(List2, data=Mat2)
#' 
#' # State space / Kalman filtering model for GBR
#' GBR <- subset(MaddisonData, (ISO=='GBR') & !is.na(gdppc))
#' # model example 
#' growthFormula <- (log(gdppc)~ -1 + SSMbespoke(growthModel(.04, GBR$gdppc) )) 
#' library(KFAS)
#' GBR2m <-SSModel(growthFormula, GBR, H=matrix(NA) )
#' 
#' # NOTE: This call ignores Time 
#' GBRgrowthFit1 <- fitSSM(GBR2m, inits=-6, method = "BFGS", 
#'                         updatefn = growthUpdateFn)
#' # NOTE: This call currently also ignores Time; MUST BE FIXED
#' GBRgrowthFit1t <- fitSSM(GBR2m, inits=-6, method = "BFGS", 
#'                          updatefn = growthUpdateFn, Time=GBR$year)
#'                        
#' #KFS example 
#' GBR_KFS <- KFAS::KFS(GBRgrowthFit1$model)
#' GBR_KFSt <- KFAS::KFS(GBRgrowthFit1t$model)
#' GBR_KFSp0 <- ggplotPath2(GBR_KFS)
#' GBR_KFSp <- ggplotPath2(GBR_KFS$a)
#' GBR_KFStp <- ggplotPath2(GBR_KFSt$a)
#' 
#' # label the lines
#' ISOll1 <- data.frame(x=c(1500, 1800), y=c(2.5, 1.7), 
#'                  label=c('GBR', 'Napoleon'), srt=c(0, 30),
#'                  col=c('red', 'green'), size=c(2, 9), 
#'                  component=1)
#'
#' GBR_KFSp1 <- ggplotPath2(GBR_KFS$a, labels=ISOll1)
#'                  
#' GBR_KFSp <- ggplotPath2(GBR_KFS, labels=ISOll1)
#' ISOll2 <- ISOll1
#' ISOll2$component <- 1:2
#' GBR_KFSp2 <- ggplotPath2(GBR_KFS, labels=ISOll2)
#' 
#' # hlines, vlines 
#' zero <- 0
#' attr(zero, 'color') <- 'red'
#' attr(zero, 'lty') <- 'dashed'
#' Hlines1 <- list(c(1,3, 10, 30), zero) 
#' Vlines <- c(1649, 1929, 1933, 1945)
#' 
#' GBR_KFSp3 <- ggplotPath2(GBR_KFS, labels=ISOll2, hlines=Hlines1, 
#'                          vlines=Vlines)
#' 
#' @export
ggplotPath2 <- function(object, ...){
  # generic function  
  UseMethod('ggplotPath2') 
}

#' @rdname ggplotPath2
#' @export
ggplotPath2.list <- function(object, ...){
##
## 1. Is this a list with a 'model' component, e.g., returned by KFAS::fitSSM, 
##    or a list of lists of args for calls to ggplotPath?
##  
  Names <- names(object)
  if('model' %in% Names){
    if('a' %in% names(object$model)){
      return(ggplotPath2(object$model$a, ...))
    }
    msgoo <- paste("'object' has component 'model', but 'model' does not ", 
                   "have component 'a'. Need to call KFAS::KFS?")
  }
##
## 2. A list of lists of args for calls to ggplotPath
##  
  mgn <- 0
  k <- length(object)
  dots <- list(...)
  Dots <- names(dots)
  mtn <- 1
  List <- vector(mode='list', length=k)
  names(List) <- Names
  for(i in 1:k){
    listi <- object[[i]]
    seli <- !(Dots %in% names(listi))
    listi2 <- c(listi, dots[seli])
    logy <- listi2$logy
    if(!is.null(logy)){
      if(!is.logical(logy)){
        if(logy=='exp_log'){
          listi2$data[, listi2[[2]]] <- exp(listi2$data[, listi2[[2]]])
          listi2$logy <- TRUE 
        } else if(logy=='log'){
          listi2$logy <- TRUE 
        } else if(logy==''){
          listi2$logy <- FALSE 
        } else {
          msg <- paste("Error for variable ", Names[i], 
                       "; logy must be either 'exp_log' or 'log' or '';", 
                      ' is ', logy)
          stop(msg)
        }
      }
    }
    Pi <- do.call(ggplotPath, listi2)
    if(i<k){ 
      Pi2 <- (Pi + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.title.x = ggplot2::element_blank(), 
                         plot.margin=ggplot2::margin(t=mgn, b=mgn)))
    } else {
      Pi2 <- (Pi + ggplot2::theme(plot.margin=ggplot2::margin(t=mgn, b=mgn)))
    }
    List[[i]] <- Pi2
  }
  List$ncol <- 1
  do.call(egg::ggarrange, List)
}

#' @rdname ggplotPath2
#' @export
ggplotPath2.mts <- function(object, Time, ...){
  Obj <- as.data.frame(object)
  N <- NROW(Obj)
  if(missing(Time)){
    Tm <- as.numeric(stats::time(object))
  } else {
    if(!is.numeric(Time)){
      stop('Time must be numeric; is ', paste(class(Time), collapse=', '))
    }
    Tm <- Time
    nT <- length(Tm)
    if(nT != (N-1)){
      stop('length(time) = ', nT, ', must be 1 less than nrow(object) = ', N, 
           '; is not.')
    }
  }
  Args <- list(object=Obj, Time=Tm)
  Dots <- list(...)
  if(length(Dots)>0) Args <- c(Args, Dots) 
  do.call(ggplotPath2, Args)
}

#' @rdname ggplotPath2
#' @export
ggplotPath2.KFS <- function(object, ...){
  if(is.null(object$a)){
    stop("A KFS object fed to ggplotPath2 must have a component 'a'; does not")
  }
  ggplotPath2(object$a, ...)
}

#' @rdname ggplotPath2
#' @export
ggplotPath2.default <- function(object, Time, object2, scaley, logy, ylab, 
        hlines, vlines=numeric(0), labels, fontsize=10, color, linetype, ...){
##
## 1. object and time? 
##  
  Object <- as.data.frame(object)
  N <- NROW(Object)
  yNames <- colnames(object)
  if(missing(Time)){
    Tim <- rownames(Object)
    Tm <- suppressWarnings(as.numeric(Tim))
    if(any(is.na(Tm))){
      warning('Time not supplied; using 1:', N)
      Tm <- 1:N
    }
  } else {
    Tm <- suppressWarnings(as.numeric(Time))
    if(any(is.na(Tm))){
      warning('time must be numeric. Is not. Using 1:', N)
      Tm <- 1:N
    }
  }
  k <- ncol(Object)
  if(k<1){
    stop('object has 0 columns.')
  }
  if(k<2){
    if(is.null(yNames)){
      if(missing(ylab)){
        yNames <- 'level'
      } else {
        if(length(ylab) != 1){
          stop("'object' has 1 column, ", N, " rows, no names, and ", 
             "length(ylab) = ", length(ylab), "; != 1.")
        } 
        if(!is.character(ylab)){
          stop("'object' has 1 column, ", N, " rows, no names, and 'ylab' is ", 
             "not character. class(ylab) = ", 
             paste(class(ylab), collapse=', ') )
        yNames <- ylab 
        }
      }
    }
    objWarn <- paste('object has 1 column and ', N, ' rows. ', 
                     'Error or call ggplotPath directly?')
    stop(objWarn)
  }
  List <- vector(mode='list', k)
  names(List) <- yNames
  nT <- length(Tm)
  if(nT==N){ 
    Data <- cbind(Object, Time=Tm)
  } else if((N-nT)==1){ 
    Data <- cbind(utils::tail(Object, -1), Time=Tm)
  } else {
    stop("'Object' and 'Time' not compatible. NROW(Object) = ", N, 
         "; length(Time) = ", nT)
  }
  names(Data) <- c(yNames, 'Time')
##
## 2. object2 
##
  if(!missing(object2)){
    Nobj2 <- NROW(object2)
    if(Nobj2 != N){
      stop('object2 has ', Nobj2, ' rows; must equal nrow(object) = ', 
                    N, '; does not.')
    }
    k2 <- NCOL(object2)
    if(k2>k){
      stop('object2 has ', k2, ' columns; must be at most ncol(object) = ', 
           k, '; is not.')
    }
    if(!is.numeric(unlist(object2))){
      stop('class(object2) must be numeric; is ', class(unlist(object2)))
    }
    if(k2<2){
      Object2 <- list(object2)
    } else Object <- object2 
  } else k2 <- 0
##
## 3. scaley 
##  
  if(missing(scaley)) {
    scaley2 <- ((k==2) && (yNames[1] == 'level') && 
                  (yNames[2] %in% (c('slope', 'growthRate'))) )
    if(scaley2){
      scaley <- c(1000, 100)
    }
    else {scaley <- rep(1, k)}
  } else {
    ks <- length(scaley)
    if(ks != k){
      msg2 <- paste('length(scaley) = ', ks, '; should equal the number of ', 
                    'columns of object, which = ', k)  
      stop(msg2)
    }
  }
##
## 4. logy
##  
  if(missing(logy)){
    logy <- c('exp_log', rep('', length=k-1))
    Logy <- c(TRUE, rep(FALSE, length=k-1))
  } else {
    kl <- length(logy)
    if(kl != k) {
      msg3 <- paste('length(logy) = ', kl, '; should equal the number of ', 
                    'columns of object, which = ', k)  
      stop(msg3)
    }
    logyVals <- c('exp_log', 'log', '')
    logyEr <- !(logy %in% logyVals)
    if(any(logyEr)){
      stop("logy can only be 'exp_log', 'log', or ''; found ", 
                       paste(logy[logyEr], collapse=' & '))
    } else {
      Logy <- rep(FALSE, length=length(logy))
      Logy[logy!=''] <- TRUE
    }
  }
##
## 5. ylab
##  
  if(missing(ylab)){
    ylab <- yNames
  } else if(is.numeric(ylab)){
    ylab <- as.character(ylab)
  } else if (!is.character(ylab)){
    stop('ylab must be a character vector; is ', class(ylab))
  }
  ky <- length(ylab)
  if(ky>k){
    stop('ylab has ', ky, ' names, more than the number of ', 
                      'columns of object = ', k)
  }
  Ylab <- ylab
  if(ky < k){
    Ylab[(ky+1):k] <- yNames[(ky+1):k]
  }
##
## 6. hlines
##  
  if(missing(hlines)){
    hlines <- vector(mode='list', length=k)
  } else if(!is.list(hlines)){
    stop('hlines must be a list, is ', paste(class(hlines), collapse=', '))
  } 
  kh <- length(hlines)
  if(kh>k){
    stop('hlines has length = ', kh, ', more than the number of ', 
         'columns of object = ', k)
  }
  Hlines <- hlines 
  if(kh < k){
    Hlines[(kh+1):k] <- list(numeric(0))
  }
##
## 7. vlines
##
  if(!is.numeric(vlines)){
    stop('vlines should be numeric; is ', paste(class(hlines), collapse=', '))
  }
##
## 8. labels
## 
  if(!missing(labels)){
    if(!is.data.frame(labels) && !is.matrix(labels)){
      stop('labels must be a data.frame; is ', 
           paste(class(hlines), collapse=', '))
    }
    Labels <- as.data.frame(labels)
    rqdLbls <- c('x', 'y', 'label','component')
    chkLbls <- (rqdLbls %in% names(Labels))
    if(any(!chkLbls)){
      msgLbls0 <- paste('labels must have column')
      if(sum(!chkLbls)>1) msgLbls0 <- paste0(msgLbls0, 's')
      missingLbls <- paste(rqdLbls[!chkLbls], collapse=' & ')
      msgLbls <- paste(msgLbls0, missingLbls)
      stop(msgLbls, '; do not.')
    }
    optLbls <- c(rqdLbls, 'srt', 'col', 'size')
    chkOpt <- (names(Labels) %in% optLbls)
    if(any(!chkOpt)){
      notUsed <- paste(names(Labels)[!chkOpt], collapse=', ')
      warning('names(labels) include ', notUsed, 
                        ': not used; ignored.')
    }
  } 
##
## 9. fontsize
##
  if(!is.numeric(fontsize)){
    stop('fontsize must be numeric; class(fontsize) = ', 
         paste(class(fontsize), collapse=', '))
  }
##
## 10. color 
##
  if(!missing(color)){
    kc <- length(color)
    if(kc>k){
      stop('length(color) = ', kc, '; > ncol(object) = ', k)
    }
    if((!is.list(color)) && !is.vector(color)){
      stop("'color' should be list or a vector; is ", 
           paste(class(color), collapse=', '))
    }
    if(!is.list(color)){
      Color <- as.list(color)
    } else Color <- color
  } 
##
## 11. linetype 
##
  if(!missing(linetype)){
    klt <- length(linetype)
    if(klt>k){
      stop('length(linetype) = ', klt, '; > ncol(object) = ', k)
    }
    if((!is.list(linetype)) && !is.vector(color)){
      stop("'linetype' should be list or a vector; is ", 
           paste(class(linetype), collapse=', '))
    }
    if(!is.list(linetype)){
      Linetype <- as.list(linetype)
    } else Linetype <- linetype
  } 
##  
## 12. Create a list of lists and call ggplotPath2.list
##
  callList <- vector(mode='list', length=k)
  names(callList) <- yNames
  for(i in 1:k){
    Dati <- Data[c('Time', yNames[i])]
    if(i <= k2) {
      Dati2 <- cbind(Dati[1], Object2[i], grp=2) 
      names(Dati2)[1:2] <- names(Dati)
      Dati <- rbind(cbind(Dati, grp=1), Dati2) 
      grpi <- 'grp'
    } else grpi <- NULL 
    if(logy[i]=='exp_log')Dati[2] <- exp(Dati[2])
    callList[[i]] <- list(x='Time', y=yNames[i], data=Dati)
    callList[[i]]$group <- grpi 
    callList[[i]]$scaley <- scaley[i]
    callList[[i]]$logy <- Logy[i]
    callList[[i]]$ylab <- Ylab[i]
    callList[[i]]$hlines <- Hlines[[i]]
    callList[[i]]$vlines <- vlines 
    if(!missing(labels)){ 
      if(!("component" %in% colnames(Labels))){
        stop("'component' not in names(labels)")
      }
      callList[[i]]$labels <- subset(Labels, component==i)
    }
    callList[[i]]$fontsize <- fontsize
    if(!missing(color))callList[[i]]$color <- Color[[i]]
    if(!missing(linetype))callList[[i]]$linetype <- Linetype[[i]]
  }
  ggplotPath2(callList, ...)
}   
