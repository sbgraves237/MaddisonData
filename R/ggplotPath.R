#' `ggplot` paths 
#'
#' `ggplotPath` plots `y` vs. `x` (typically `year`) with a separate line for 
#' each group with options for legend placement, horizontal and vertical lines 
#' and labels.  
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
#' product (GDP) per capita in 2011 dollars at purchasing power parity (PPP), 
#' for which we typically want `scaley` = 1000. 
#' @param logy logical: if `TRUE`, y axis is on a log scale; default = `TRUE`.`
#' @param legend.position argument passed to [`theme`]. If `!missing(labels)`, 
#' default is no legend. Otherwise, default depends on 
#' `nGps <- length(unique(data[, group]`: If `nGps` = 1, there is no legend. If 
#' `nGps > 10`, `legend.position = 'right'`. In between, `legend.position` = 
#' c(.15, .5) = center left. For alternatives, see [`ggplot2::theme`]. 
#' @param hlines numeric vector of locations on the `y` axis for horizontal 
#' lines using `ggplot2::geom_hline(yintercept = hlines, ...)` 
#' with `color='grey', lty='dotted'` unless `color` or `colour` and / or `lty` 
#' are available as `attr(x, ...)`.  
#' @param vlines numeric vector of locations on the `x` axis for vertical lines 
#' using `ggplot2::geom_vline(xintercept = vlines, ...)` 
#' with `color='grey', lty='dotted'` unless `color` or `colour` and / or `lty` 
#' are available as `attr(x, ...)`.  
#' @param labels = [`data.frame`] with columns `x, y, label`, and optionally 
#' `srt, col, size`, where `x`, `y`, `srt`, and `size` are are numeric, 
#' `label` is character, and `col` are acceptable values for `color` in 
#' `with(labels, annotate('text', x=x, y=y, label = label, srt=srt, 
#' color=col, size=size))`. Defaults for `srt`, `col`, and `size`  are 0, 
#' 'black', and 4, respectively. 
#' @param fontsize for legend and axes labels in 
#' theme(text=element_text(size=fontsize)); default = 10. 
#' 
#' @returns an object of class [`ggplot2::ggplot`], which can be subsequently 
#' edited, and whose [`print`] method produces the desired plot. 
#' 
#' @export
#'
#' @examples
#' str(GBR_USA <- subset(MaddisonData::MaddisonData, ISO %in% c('GBR', 'USA')))
#' GBR_USA1 <- MaddisonData::ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000)
#' 
#' GBR_USA1+ggplot2::coord_cartesian(xlim=c(1500, 1850)) # for only 1500-1850 
#' GBR_USA1+ggplot2::coord_cartesian(xlim=c(1600, 1700), ylim=c(7, 17)) 
#' 
#' # label the lines
#' ISOll <- data.frame(x=c(1500, 1800), y=c(2.5, 1.7), label=c('GBR', 'USA'), 
#'               srt=c(0, 30), col=c('red', 'green'), size=c(2, 9))
#' GBR_USA2 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
#'                     labels=ISOll, fontsize = 20)  
#'                         
#' # h, vlines, manual legend only 
#' Hlines <- c(1,3, 10, 30)
#' Vlines = c(1849, 1929, 1933, 1939, 1945)
#' (GBR_USA3 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
#'        legend.position = NULL, hlines=Hlines, vlines=Vlines, labels=ISOll))  
#' 
#' @keywords plot
ggplotPath <- function(x='year', y, group, data, scaley=1, logy=TRUE, 
                       legend.position, hlines, vlines, labels, fontsize=10){
##
## 1. check x and data 
## 
#  localTrace <- TRUE
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
#  if(localTrace){
#    cat('Check group: x = ', x, '; y = ', y, '\n')
#  }
  if(missing(group)){
#    p0 <- ggplot2::ggplot(dat, ggplot2::aes(X=ggplot2::.data[[x]], 
#                                            Y=ggplot2::.data[[y]]))
    names(dat)[c(X, Y)] <- c('x', 'y')
    p0 <- ggplot2::ggplot(dat, ggplot2::aes(x, y))
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
#    if(localTrace){
#      cat('found group = ', group, ': x = ', x, '; y = ', y, '\n')
#    }
    names(dat)[c(X, Y, Gp)] <- c('x', 'y', 'group')
#    p0 <- ggplot2::ggplot(dat, ggplot2::aes(x=ggplot2::.data[[x]], 
#                      y=ggplot2::.data[[y]], group=ggplot2::.data[[group]], 
#                      color=ggplot2::.data[[group]]))
    p0 <- ggplot2::ggplot(dat, ggplot2::aes(x=x, y=y, group=group, 
                                            color=group))
  }
  p1 <- (p0 + ggplot2::geom_path())  
  if(logy){
    p1 <- (p1 + ggplot2::scale_y_log10())
  }
##
## 5. legend.position
##  
  if(!missing(labels)){
    if(!missing(legend.position)){
      p2 <- (p1 + ggplot2::theme(legend.position=legend.position))
    } else p2 <- p1
  } else {
    if(!missing(group)){
      if(missing(legend.position)){
#       gps <- table(dat[, group])
        gps <- table(dat[, "group"])
        if(length(gps)<10) {
          p2 <- (p1 + ggplot2::theme(legend.position=c(.15, .5)))
        } else p2 <- p1 
      } else {
        p2 <- (p1 + ggplot2::theme(legend.position=legend.position))
      }
    } else p2 <- p1 
  }
##
## 6. hlines
##  
  if(!missing(hlines)){
    p2 <- p2 +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill='transparent'), #transparent panel bg
        plot.background = ggplot2::element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = ggplot2::element_blank(), #remove major gridlines
        panel.grid.minor = ggplot2::element_blank(), #remove minor gridlines
        legend.background = ggplot2::element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = ggplot2::element_rect(fill='transparent') #transparent legend panel
      )
    hcol <- attr(hlines, 'color')
    if(is.null(hcol)) hcol <- attr(hlines, 'colour')
    hlty <- attr(hlines, 'lty')
    if(is.null(hcol)){
      if(is.null(hlty)){
        p2 <- (p2 + ggplot2::geom_hline(yintercept = hlines, 
                        col='grey', lty='dotted'))
      } else {
        p2 <- (p2 + ggplot2::geom_hline(yintercept = hlines, 
                        col='grey', lty=hlty))
      }
    } else {
      if(is.null(hlty)){
        p2 <- (p2 + ggplot2::geom_hline(yintercept = hlines, 
                        color=hcol, lty='dotted'))
      } else {
        p2 <- (p2 + ggplot2::geom_hline(yintercept = hlines, 
                        lty=hlty, color=hcol))
      }
    }
  } 
##
## 7. vlines
##  
  if(!missing(vlines)){
    p2 <- p2 +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill='transparent'), #transparent panel bg
        plot.background = ggplot2::element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = ggplot2::element_blank(), #remove major gridlines
        panel.grid.minor = ggplot2::element_blank(), #remove minor gridlines
        legend.background = ggplot2::element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = ggplot2::element_rect(fill='transparent') #transparent legend panel
      )
    col <- attr(vlines, 'color')
    if(is.null(col)) col <- attr(vlines, 'colour')
    lty <- attr(vlines, 'lty')
    if(is.null(col)){
      if(is.null(lty)){
        p2 <- (p2 + ggplot2::geom_vline(xintercept = vlines, 
                                        col='grey', lty='dotted'))
      } else {
        p2 <- (p2 + ggplot2::geom_vline(xintercept = vlines, 
                                        col='grey', lty=lty))
      }
    } else {
      if(is.null(lty)){
        p2 <- (p2 + ggplot2::geom_vline(xintercept = vlines, 
                                        color=col, lty='dotted'))
      } else {
        p2 <- (p2 + ggplot2::geom_vline(xintercept = vlines, 
                                        lty=lty, color=col))
      }
    }
  } 
##
## 8. labels
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
    if(!('size' %in% names(labels))){
      labels$size <- 4
    }
    nLbls <- nrow(labels)
    for(i in seq(length=nLbls)){
      p2 <- (p2 + ggplot2::annotate("text", x=labels$x[i], y=labels$y[i], 
                        label=labels$label[i], colour=labels$col[i], 
                        srt=labels$srt[i], size=labels$size[i]))
    }
  }
##
## 9. axis text and titles 
##
  p3 <- (p2 + ggplot2::theme(axis.title.x=
                    ggplot2::element_blank()))
  if(scaley == 1){
    y_ <- y 
  } else {
    y_ <- paste(y, scaley, sep=' / ')
  }
  p4 <- (p3 + ggplot2::labs(y=y_))
## 
## 10. axis font size 
##
  p5 <- p4 + ggplot2::theme(text=ggplot2::element_text(size=fontsize))
##
## 11. Done
##
  p5
}