#' ggplot paths 
#'
#' `ggplotPath` plots `y` vs. `x` (typically `year`) with a separate line for 
#' each group with options for legend placement, vertical lines and labels.  
#' 
#' @param x name of column in `data` to pass as `x` in 
#' `aes(x=.data[[x]], ...)`; default = `year`.
#' @param y name of column in `data` to pass as `y` in 
#' `aes(y=.data[[y]], ...)`; must be supplied. 
#' @param group name of grouping variable, i.e., plot a separate line for each 
#' level of `group` using `aes(group=.data[[group]], ...)`, unless `group` is 
#' missing or `length(unique(data[, group]))` = 1. 
#' @param data [`data.frame`] or [`tibble::tibble`] with columns `x`, `y`, and 
#' `group`. 
#' @param scaley factor to divide y by for plotting. Default = 1, but for data 
#' in monetary terms, e.g., for `MaddisonData`, `y = 'gdppc'` is Gross domestic 
#' product (GDP) per capita in 2011 dollars at puchasing power parity (PPP), 
#' for which we typically want `scaley` = 1000. 
#' @param logy logical: if `TRUE`, y axis is on a log scale; default = `TRUE`.`
#' @param legend.position argument passed to [`theme`]. Default depends on 
#' `nGps <- length(unique(data[, group]`: If `nGps` = 1, there is no legend. If 
#' `nGps > 11`, `legend.position = 'right'`. In between, `legend.position` = 
#' c(.1, .5) = center left. For alternatives, see [`ggplot2::theme`]. 
#' @param vlines = locations on the `x` axis for vertical lines using 
#' `ggplot2::geom_vline(aes(xintercept = .data[[x]]), data=vlines, ...)` with 
#' `color='grey', lty='dotted'` unless `color` or `colour` and / or `lty` are 
#' available as `attr(x, ...)`.  
#' @param labels = [`data.frame`] with columns `x, y, label, srt, col`, where 
#' `x`, `y`, and `srt` are numeric, `label` is character, and `col` are 
#' acceptable values for `color` in `with(labels, 
#' annotate('text', x=x, y=y, label = label, srt=srt, color=col))`. 
#' 
#' @returns an object of class [`ggplot2::ggplot`], which can be subsequently 
#' edited, and whose [`print`] method produces the desired plot. 
#' 
#' @export
#'
#' @examples
#' str(GBR_USA <- subset(MaddisonData::MaddisonData, ISO %in% c('GBR', 'USA')))
#' GBR_USA1 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000)
#' 
#' GBR_USA1+ggplot2::coord_cartesian(xlim=c(1500, 1850)) # for only 1500-1850 
#' GBR_USA1+ggplot2::coord_cartesian(xlim=c(1600, 1700), ylim=c(7, 17)) 
#' 
#' # label the lines
#' ISOll <- matrix(c(1500, 1750, 1.4, .7), 2, dimnames=
#'             list(c('GBR', 'USA'), c('x', 'yAdj')))
#' (GBR_USA2 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
#'                 labels=ISOll) ) 
#' # vlines 
#' 
#' Vlines = c(1849, 1929, 1933, 1939, 1945)
#' (GBR_USA3 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
#'                 vlines=Vlines, labels=ISOll) ) 
#' 
#' @keywords plot
ggplotPath <- function(x='year', y, group, data, scaley=1, logy=TRUE, 
                       legend.position, vlines, labels){
##
## 1. check x and data 
## 
  if(missing(x)){
    stop('x missing with no default')
  }
  if(missing(data)){
    stop('data missing with no default')
  }
  if(!inherits(data, 'data.frame')){
    stop('class(data) must include "data.frame"; is ', 
         paste(class(data), collapse=', '))
  }
  X <- which(names(data) == x)
  if(length(X) < 1){
    stop('x = ', x, ' not found in names(data) = ', 
         paste(names(data), collapse=', '))
  }
  if(length(X) > 1){
    stop('x = ', x, ' found more once in names(data) = ', 
         paste(names(data), collapse=', '))
  }
##
## 2. check y
## 
  if(missing(y)){
    stop('y missing with no default')
  }
  Y <- which(names(data) == y)
  if(length(Y) < 1){
    stop('y = ', y, ' not found in names(data) = ', 
         paste(names(data), collapse=', '))
  }
  if(length(Y) > 1){
    stop('y = ', y, ' found more once in names(data) = ', 
         paste(names(data), collapse=', '))
  }
##
## 3. Delete is.na(data[, y]) and rescale if deesired
##
  dat <- data[!is.na(data[, y]), ]
  dat[, y] <- (dat[, y]/scaley)
##
## 4. check group
## 
  if(missing(group)){
    p0 <- ggplot2::ggplot(dat, ggplot2::aes(.data[[x]], .data[[y]]))
  } else{
    Gp <- which(names(dat) == group)
    if(length(Gp) < 1){
      stop('group = ', group, ' not found in names(data) = ', 
           paste(names(dat), collapse=', '))
    }
    if(length(Gp) > 1){
      stop('group = ', group, ' found more once in names(data) = ', 
           paste(names(dat), collapse=', '))
    }
    p0 <- ggplot2::ggplot(dat, ggplot2::aes(.data[[x]], .data[[y]], 
                            group=.data[[group]], color=.data[[group]]))
  }
  p1 <- (p0 + ggplot2::geom_path())  
  if(logy){
    p1 <- (p1 + ggplot2::scale_y_log10())
  }
##
## 5. legend.position
##  
  if(!missing(group)){
    if(missing(legend.position)){
      gps <- table(dat[, group])
      if(length(gps)<11)
        p2 <- (p1 + ggplot2::theme(legend.position=c(.1, .5)))
    } else {
      p2 <- (p1 + ggplot2::theme(legend.position=legend.posiiton))
    }
  }
##
## 6. vlines
##  
  if(!missing(vlines)){
    col <- attr(vlines, 'color')
    if(is.null(col)) col <- attr(vlines, 'colour')
    lty <- attr(vlines, 'lty')
    if(is.null(col)){
      if(is.null(lty)){
        p2 <- (p2 + ggplot2::geom_vline(
                    ggplot2::aes(xintercept = vlines)))
      } else {
        p2 <- (p2 + ggplot2::geom_vline(
                    ggplot2::aes(xintercept = vlines, lty=lty)))
      }
    } else {
      if(is.null(lty)){
        p2 <- (p2 + ggplot2::geom_vline(
          ggplot2::aes(xintercept = vlines, color=col)))
      } else {
        p2 <- (p2 + ggplot2::geom_vline(
          ggplot2::aes(xintercept = vlines, lty=lty, color=col)))
      }
    }
  }
##
## 7. vlines
##  
  if(!missing(labels)){
    if(!inherits(labels, 'data.frame')){
      stop('class(labels) must include "data.frame"; is ', 
           paste(class(labels), collapse=', '))
    }
    if(!('x' %in% names(labels))){
      stop('"x" not in names(labels) = ', 
           paste(names(labels), collapse=', '))
    }
    if(!('y' %in% names(labels))){
      stop('"y" not in names(labels) = ', 
           paste(names(labels), collapse=', '))
    }
    if(!('label' %in% names(labels))){
      stop('"label" not in names(labels) = ', 
           paste(names(labels), collapse=', '))
    }
    if(!('srt' %in% names(labels))){
      labels$srt <- 0 
    }
    if(!('col' %in% names(labels))){
      labels$col <- 'black' 
    }
    nLbls <- nrow(labels)
    for(i in seq(length=nLbls)){
      p2 <- (p2 + ggplot2::annotate("text", x=labels$x[i], y=labels$y[i], 
                        label=labels$label[i], colour=labels$col[i], 
                        srt=labels$srt[i]))
    }
  }
##
## 8. axis text and titles 
##
  p3 <- (p2 + ggplot2::theme(axis.title.x=
                    ggplot2::element_blank()))
##
## 8. Done
##
  p3
}