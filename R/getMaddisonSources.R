#' Get Maddison sources  
#'
#' @description
#' The 
#' \href{https://en.wikipedia.org/wiki/Maddison_Project}{Maddison project} 
#' collates historical economic statistics from many sources. 
#' 
#' They have a citation policy: 
#' CONDITIONS UNDER WHICH ALL ORIGINAL PAPERS MUST BE CITED:
#' 
#' a) If the data is shown in any graphical form
#' b) If subsets of the full dataset that include less than a dozen (12) 
#' countries are used for statistical analysis or any other purposes
#' 
#' When neither a) or b) apply, then the MDP as a whole can be cited.
#' 
#' `getMaddisonSources` returns a [`data.frame`] of relevant sources for a 
#' particular application. 
#' 
#' @param ISO either NULL to return all sources or a character vector of ISO 
#' codes for the countries included in the analysis or a [`data.frame`] with 
#' the first column being the ISO codes followed by `yearBegin` and optionally 
#' `yearEnd`. 
#' @param plot logical indicating whether the use does nor does not include 
#' plotting data. The Maddison project requires citing all relevant 
#' `MaddisonSources` if they are plotted, denoted here by `plot` = TRUE. If no 
#' data are plotted, then the Maddison project requires citing all sources only 
#' if less than a dozen are used, denoted here by `plot` = FALSE, in which 
#' case, the Maddison project requires a specific project-level citation. 
#' Default = TRUE. 
#' @param sources list of sources in the format of [`MaddisonSources`]; 
#' default is `MaddisonSources`. 
#' @param years `data.frame` in the format of [`MaddisonYears`]; default is 
#' `MaddisonYears`. 
#' 
#' @returns a [`tibble::tibble`] with 3 columns:
#' \describe{
#'   \item{ISO}{3-letter ISO code for country.}
#'   \item{years}{
#'    character vector of years or year ranges for which `source` applies. 
#'   }
#'   \item{source}{character vector of sources.}
#' }
#' 
#' in the format of [`MaddisonSources`].  
#' 
#' @export
#'
#' @examples
#' getMaddisonSources() # all 
#' as.data.frame(GBR) # display all; default = tibble
#' 
#' getMaddisonSources(plot=FALSE) # only MDP 
#' getMaddisonSources('GBR') # GBR 
#' getMaddisonSources(names(MaddisonSources)[1:12], FALSE) # only MDP 
#' getMaddisonSources(data.frame(ISO=c('GBR', 'USA'), 
#'              yearBegin=rep(1500, 2)) ) #GBR, USA since 1500 
#' getMaddisonSources('AUS') # AUS: no special sources for AUS. 
#' 
#' @keywords manip 
getMaddisonSources <- function(ISO=NULL, plot=TRUE, 
    sources=MaddisonData::MaddisonSources, years=MaddisonData::MaddisonYears){
##
## 1. check ISO
## 
  ISO0 <- FALSE 
  if(is.null(ISO))ISO0 <- TRUE
  if(NROW(ISO)<1)ISO0 <- TRUE    
  if((!plot) && (ISO0 || NROW(ISO)>11)){
    MDPonly_ <- tibble::tibble(ISO='', years='1, .., 2022', 
      source=paste('Bolt and Van Zanden (2024)', 
        '"Maddison style estimates of the evolution of the world economy:',
        'A new 2023 update", Journal of Economic Surveys, 1-41') )
    return(MDPonly_)
  } else {
## 
## 2. Get ISOsources
##
    if(ISO0) ISO <- names(sources)
    if(NCOL(ISO)>1){
      ISO1 <- ISO[, 1, drop=TRUE]
    } else ISO1 <- ISO
    ISOsrc <- tibble::tibble(ISO=character(0), 
                             year=character(0), 
                             source=character(0))
    for(iso in ISO1){
      isoSrc <- sources[[iso]]
      if(NCOL(ISO)>1){
        yrsIso <- (years[, 1]== iso)
        selIso <- (ISO[, 1]==iso) 
# no overlap if y2 < iy1         
        
        nosel <- (years[yrsIso, 3] < ISO[selIso, 2])
        if(NCOL(ISO)>2){
# Also no overlap if iy2 < y1           
          nosel <- (nosel | (ISO[selIso, 3] < years[yrsIso, 2]))
        }
        isoSrc1 <- isoSrc[!nosel, ]
      } else isoSrc1 <- isoSrc
      if(!is.null(isoSrc1)){ 
        isoS <- cbind(ISO=iso, isoSrc1)
        ISOsrc <- rbind(ISOsrc, isoS)
      }
    }
## 
## 3. Add boilerplate
##    
    MDP1 <- tibble::tibble(ISO=character(2), 
              years=c('2008-', '1990-'), 
              source=c(paste('GDP pc:    Total Economy Database (TED) of the',
                  'Conference Board for all countries included in TED.', 
                  'Otherwise UN national accounts statistics'), 
                paste('population:Total Economy Database (TED) of the', 
                  'Conference Board for all countries included in TED.', 
                  'Otherwise UN national accounts statistics') ) )
    srcOut <- rbind(MDP1, ISOsrc)    
    return(srcOut)
  }
##
## 4. Done
## 
}