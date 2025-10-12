test_that("MaddisonSources", {
  expect_lte(length(MaddisonSources), 
               nrow(MaddisonCountries)) 
  since2008 <- paste("gdppc since 2008: Total Economy Database (TED)", 
      "from the Conference Board for all countries included in TED and UN", 
      "national accounts statistics for all others.")
  expect_equal(attr(MaddisonSources, 'since2008'), 
               since2008)
})

test_that("MaddisonYears", {

  GBR <- MaddisonYears[MaddisonYears$ISO=='GBR', ] 
  rownames(GBR) <- NULL 
  GBRe <- data.frame(
      ISO=rep('GBR', 3), 
      yearBegin=c(1, 1252, 1700), 
      yearEnd  =c(1, 1700, 1870), 
      sourceNum=1:3
    ) 
  expect_equal(GBR, GBRe) 

  EGY <- MaddisonYears[MaddisonYears$ISO=='EGY', ] 
  rownames(EGY) <- NULL 
  EGYe <- data.frame(
      ISO=rep('EGY', 6), 
      yearBegin=c(1,  700, 1820, 1870, 1913, 1950), 
      yearEnd  =c(1, 1500, 1820, 1870, 1913, 1950), 
      sourceNum=c(1,    2, rep(3, 4))
    ) 
  expect_equal(EGY, EGYe) 
})
