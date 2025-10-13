test_that("logMaddison", {
  expect_error(logMaddison('x'))
#  
  MDPa_ <- MaddisonData
  MDPa_$lnGDPpc <- log(MDPa_$gdppc)
  MDPa_$lnPop <- log(MDPa_$pop)
  expect_equal(logMaddison(), MDPa_)
#  
  GBR_USA <- MaddisonData[MaddisonData$ISO %in% c('GBR', 'USA'), ]
  GBR_USA$lnGDPpc <- log(GBR_USA$gdppc)
  GBR_USA$lnPop <- log(GBR_USA$pop)  
  expect_equal(logMaddison(c('GBR', 'USA')), GBR_USA) 
})
