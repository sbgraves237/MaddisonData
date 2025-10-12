#' Construct a path to a location within an installed or development package
#'
#' `path_package2` returns a character vector of matches to `target`.
#' It differs from [system.file()] in that it supports searching for a `target`
#' file or folder possibly in `subdirs` of the working directory or in 
#' `nparents` of its parents.  
#' 
#' This works in a vignette searching for a `target` that could be in the 
#' `vignettes` directory of its parent package or in the package directory 
#' or in, e.g., one of `subdirs = c('extdata', 
#' paste('inst', 'extdata',  sep=.Platform$file.sep))`. 
#' 
#' Returns the full path to match(s) if found and a character vector of length 
#' 0 if no matches are found. The returned object also has a `searched` 
#' attribute being a character vector of the directories searched. 
#' 
#' This was inspired by a desire to share with others a vignette describing how 
#' to create data objects from a file that could not itself be shared on CRAN. 
#' This is not easy, because the working director available to code in a 
#' vignette changes depending on how that code is run. 
#' 
#' `path_package2` allows the user to store the `target` locally, e.g., in 
#' `inst/extdata` but include it in `.gitignore` to prevent it from leaving the 
#' local computer. The vignette then decides what to do after calling 
#' `path_package2()` based on the length of the the object returned. 
#'
#' @param target A regular expression describing the file of folder desired. 
#' @param package Name of the package to in which to search. If `NULL`, search 
#' in the working directory. Otherwise search in `system.file(package)`. 
#' @param nparents integer indicate the number of parents of the working 
#' directory in which to search; default = 1. 
#' @param subdirs = c('extdata',  
#'                     paste('inst','extdata', sep=.Platform$file.sep))
#' @returns a character vector with an attribute `searched` giving the full 
#' paths of all directories searched for `target`. 
#' @export
#'
#' @examples
#' # search for a file matching a regular expression
#' path_package2('^mpd.*xlsx$') 
#' # search only in the working directory
#' path_package2('^mpd.*xlsx$', nparents=0, subdirs=character(0)) 
#' 
#' @keywords file 
path_package2 <- function(target, package=NULL, nparents=1, 
    subdirs = c('extdata', 
        paste('inst','extdata', sep=.Platform$file.sep))){
  s1 <- if(is.null(package)) getwd() else system.file(package)
  searched <- character(0)
# Define a function to search subdirs  
  Search <- function(Dir=s1){
    searched <- c(searched, Dir)
    F_ <- dir(Dir, target, full.names = TRUE) 
    for(i in seq(length=length(subdirs))){
      if(length(F_)<1){
        Di <- file.path(Dir, subdirs[i])
        searched <- c(searched, Di)
        F_ <- dir(Di, target, full.names=TRUE)
        if(length(F_)>0)break
      }
    }
    attr(F_, 'searched') <- searched
    F_
  }
  Fl <- Search(s1)
  searched <- attr(Fl, 'searched')
  if(length(Fl)<1){
    splitDir <- strsplit(s1, .Platform$file.sep)[[1]]
    for(i in seq(length=nparents)){
      Di <- paste(utils::head(splitDir, -i), 
                    collapse=.Platform$file.sep)
      Fl <- Search(Di)
      searched <- c(searched, attr(Fl, 'searched'))
      if(length(Fl)>0) break 
    }
  }
  attr(Fl, 'searched') <- searched
  Fl
}