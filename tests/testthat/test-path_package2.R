test_that("path_package2", {
# search for a file matching a regular expression
# Example 1  
  mpdXlsx <- path_package2('^mpd.*xlsx$') 
  searched <- attr(mpdXlsx, 'searched')
# A call to "expect*" seems to be needed to trigger testing    
  expect_equal(class(mpdXlsx), 'character')
  if(length(mpdXlsx)>0){
    mpdList <- strsplit(mpdXlsx, .Platform$file.sep)[[1]]
    lengrepxlsx <-length(grep('^mpd.*xlsx$', tail(mpdList, 1)))
    if(lengrepxlsx<1){
      print(mpdXlsx)
      cat("object returned by path_package2('^mpd.*xlsx$')", 
         ' did not contain the search string.\n')
      expect_gt(lengrepxlsx, 0)  
    }
    if(length(searched)<1){
        cat("object returned by path_package2('^mpd.*xlsx$')", 
           ' did not have a "searched" attribute.\n')
        expect_gt(length(searched), 0)
    }
  }
  searched1 <- strsplit(searched[1], .Platform$file.sep)[[1]]
  lengrepSS4 <- length(grep('MaddisonData', searched1))
  if(lengrepSS4<1){
    print(searched1)
    cat("'searched' attribute of object returned by ", 
             "path_package2('^mpd.*xlsx$')[1] did not include ", 
             'MaddisonData\n')
    expect_gt(lengrepSS4, 0)
  }
# Example 2  
  mpdXlsx0 <- path_package2('^mpd.*xlsx$', nparents=0, subdirs=character(0))
  searched0 <- attr(mpdXlsx0, 'searched')
  expect_equal(class(mpdXlsx0), 'character')
  if(length(mpdXlsx0)>0){
    mpdList <- strsplit(mpdXlsx0, .Platform$file.sep)[[1]]
    lengrepXls0 <- grep('^mpd.*xlsx$', tail(mpdList, 1))
    if(lengrepXls0<1){
      print(mpdXlsx0)
      cat("object returned by path_package2('^mpd.*xlsx$',",
           " nparents=0, subdirs=character(0)) did not contain", 
           ' the search string.\n')
      expect_gt(lengrepXls0, 0)
    }
  }
  if(length(searched0)!=1){
    print(mpdXlsx0)
    cat("object returned by path_package2('^mpd.*xlsx$',",
         " nparents=0, subdirs=character(0)) should have length 1;" 
         , " had length = ", length(searched0), '\n')
    expect_eq(length(searched0), 1)
  }
  searched01 <- strsplit(searched0[1], .Platform$file.sep)[[1]]
  lenSearch01 <- length(grep('MaddisonData', searched01))
  if(lenSearch01<1){
    print(searched0)
    cat("'searched' attribute of object returned by ", 
             "path_package2('^mpd.*xlsx$', nparents=0, ", 
             "subdirs=character(0)) did not contain 'MaddisonData'.")
    expect_gt(lenSearch01, 0)
  } 
})
