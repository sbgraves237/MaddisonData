#' Plot selected countries
#'
#' `plotMaddison` plots y vs. year with a separate line for each ISO country 
#' code. 
#' 
#' @param ISO either NULL to select all the data in `MaddisonData` or a 
#' character vector of `ISO` codes used in the Maddison project. 
#' @param y name of column in `data` to plot. Default = 1. 
#' @param lty,col,lwd line types, colors, and widths for the different lines, 
#' one for each ISO. Defaults for `lty` and `col` = 1:length(`ISO`). Default 
#' for `lty`  = 1. 
#' @param data [`data.frame`] or [`tibble::tibble`] with first two columns 
#' being `ISO` and `year` and `y` being the name of another column. 
#' @param scaley factor to divide y by for plotting. Default = 1000, e.g., so 
#' numbers in thousands are plotted in millions. 
#' @param logy logical: if `TRUE`, y axis is on a log scale; default = `TRUE`.`
#' @param Size size of axis text labels, tick labels and ISO code in a named 
#' 3-vector with default = c(axLab=14, tickLabs=11, ISO=14). 
#' @param ISOlabelLoc location of line labels as a matrix with rownames = ISO 
#' codes and columns `x` and `yAdj`: The algorithm finds the closest points 
#' above and below `x` and averages both `x` and `y` for the 1, 2, or 3 points 
#' found bracketing `x`, then multiplies the average `y` by `yAdj` and plots 
#' the `ISO` label there with `col` specified for that `ISO` code. If 
#' `ISOlabelLoc` is missing or `ISO` is not found 
#' among the rownames of `ISOlabelLoc`, that line is not labeled. 
#' 
#' @returns an object of class [`ggplot2::ggplot`], which can be subequently 
#' edited, and whose [`print`] method produces the desired plot. 
#' 
#' @export
#'
#' @examples
#' (GBR_USA <- plotMaddison(c('GBR', 'USA'))) # GBR, USA
#' GBR_USA+ggplot2::coord_cartesian(xlim=c(1500, 1850)) # for only 1500-1850 
#' GBR_USA+ggplot2::coord_cartesian(xlim=c(1600, 1700), ylim=c(7, 17)) 
#' 
#' ISOll <- matrix(c(1500, 1750, 1.4, .7), 2, dimnames=
#'             list(c('GBR', 'USA'), c('x', 'yAdj')))
#' (GBR_USA_ <- plotMaddison(c('GBR', 'USA'), ISOlabelLoc=ISOll) ) 
#' # label the lines
#' 
#' @keywords plot
plotMaddison <- function(ISO, y, lty, col, lwd, 
          data=MaddisonData::MaddisonData, scaley=1000, 
          logy=TRUE, Size=c(text=14, axes=11, ISO=14), 
          ISOlabelLoc){
##
## 1. check ISO
## 
  if(missing(ISO)){
    stop('ISO missing with no default')
  }
  chkISO <- sapply(ISO, function(x){
    sum(data[, 1] == x)
  })
  if(min(chkISO)<1){
    ISO0 <- names(chkISO[chkISO<1])
    stop('No matches found for ISO = ', 
         paste(ISO0, collapse = ', '))
  }
##
## 2. Check y 
## 
  if(NCOL(data)<3){
    stop('data must have at least 3 columns; has ', 
         NCOL(data))
  }
  if(missing(y)) {Y <- 3} else {
    if(length(y) != 1){
      print(y)
      stop('length(y) must be 1; is ', length(y))
    }
    if(!(y %in% names(data))){
        stop('y = ', y, ' not found in names(data) ')
    } else { 
      Y <- which(names(data) == y)
    }
  }
##
## 3. lty, col, lwd
##
  nISO <- length(ISO)
  if(missing(lty)){
    lty <- 1:nISO
  } else lty <- rep(lty, len=nISO)
  if(missing(col)){
    col <- 1:nISO
  } else col <- rep(col, len=nISO)
  if(missing(lwd)){
    lwd <- rep(1, nISO)
  } else lwd <- rep(lwd, len=nISO)
##
## 4. check, revise data 
##
  if(length(intersect(class(data), c('data.frame', 'tibble')))<1){
    stop('data is neither a tibble nor a data.frame; is ',
         class(data) ) 
  }
  dat0 <- cbind(data[, c(1:2)], data[, Y]/scaley)
  names(dat0) <- c('ISO', 'x_', 'y_')
  dat <- dat0[!is.na(dat0[, 3]), ]
##
## 5. plot ISO
##  
  sel1 <- (dat[, 1] == ISO[1])
  dat1 <- dat[sel1, 2:3]
  if(logy){
    p1 <- (ggplot2::ggplot(dat1, ggplot2::aes(x_, y_)) 
           + ggplot2::scale_y_log10()
           + ggplot2::geom_path(color = col[1], linetype = lty[1], 
                                linewidth = lwd[1]))
  } else {
    p1 <- (ggplot2::ggplot(dat1, ggplot2::aes(x_, y_)) 
         + ggplot2::geom_path(color = col[1], linetype = lty[1], 
                     linewidth = lwd[1]))
  }
  if(!missing(ISOlabelLoc)){
    ISOll1 <- which(rownames(ISOlabelLoc) == ISO[1])
    if(length(ISOll1)>0){
      xlow <- which(dat1[, 1] < ISOlabelLoc[ISOll1, 1])
      xhigh <- which(dat1[, 1] > ISOlabelLoc[ISOll1, 1])
      if((length(xlow)>0) || (length(xhigh)>0)) { 
        if(length(xlow)<1){
          x0 <- (min(xhigh)-1:0)
        } else {
          if(length(xhigh)<1){
            x0 <- (max(xlow)+0:1)
          } else {
            x0 <- c(max(xlow), min(xhigh))
          }
        }
      }
      selx <- (x0[1]:x0[2])
      x_ <- mean(dat1[selx, 1])
      y_ <- (mean(dat1[selx, 2])*ISOlabelLoc[ISOll1, 2])
      p1 <- (p1 + ggplot2::annotate("text", x=x_, y=y_, label=ISO[1], 
                                    colour=col[1]))
    }
  }
  if(nISO>1)for(i in 2:nISO){
    seli <- (dat[, 1] == ISO[i])
    dati <- dat[seli, 2:3]
    p1 <- (p1 + ggplot2::geom_path(data=dati, ggplot2::aes(x_, y_), 
                  color = col[i], linetype = lty[i], 
                  linewidth = lwd[i]))
    if(!missing(ISOlabelLoc)){
      ISOlli <- which(rownames(ISOlabelLoc) == ISO[i])
      if(length(ISOlli)>0){
        xlowi <- which(dati[, 1] < ISOlabelLoc[ISOlli, 1])
        xhighi <- which(dati[, 1] > ISOlabelLoc[ISOlli, 1])
        if((length(xlowi)>0) || (length(xhighi)>0)) { 
          if(length(xlowi)<1){
            x0i <- (min(xhighi)-1:0)
          } else {
            if(length(xhighi)<1){
              x0i <- (max(xlowi)+0:1)
            } else {
              x0i <- c(max(xlowi), min(xhighi))
            }
          }
        }
        selxi <- (x0i[1]:x0i[2])
        x_i <- mean(dati[selxi, 1])
        y_i <- (mean(dat1[selxi, 2])*ISOlabelLoc[ISOlli, 2])
        p1 <- (p1 + ggplot2::annotate("text", x=x_i, y=y_i, label=ISO[i], 
                                      color=col[i]))
      }
    }
  }         
##
## 6. axis text and titles 
##
  p2 <- (p1 + ggplot2::theme(axis.title.x=
                    ggplot2::element_blank()))
  p3 <- (p2 + ggplot2::labs(y=names(data)[Y]) + 
         ggplot2::theme(axis.title = 
                ggplot2::element_text(size = Size['text'])))
  p4 <- (p3 + ggplot2::theme(axis.text=
                  ggplot2::element_text(size = Size['axes']) ))
##
## 7. legend
##
  p5 <- (p4 + ggplot2::theme(legend.position = 'inside', 
                legend.position.inside=c(.1, .9), 
                legend.justification = c("left", "top"),
                legend.box.just = "right",
                legend.margin = ggplot2::margin_auto(6)
                ))
##
## 8. Done
##
  p5
}