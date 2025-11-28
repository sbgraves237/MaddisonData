test_that("ggplotPath", {
  expect_error(ggplotPath())
  expect_error(ggpotPath(c('GBR', 'x')))
  expect_error(ggpotPath(c('GBR', 'x'), character(0)))
  expect_error(ggpotPath('GBR', 'illegal y'))
  expect_error(ggpotPath('GBR', c('long', ' y')) )

  GBR_USA <- subset(MaddisonData::MaddisonData, ISO %in% c('GBR', 'USA'))
  GBR_USA1 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, scaley=1000)
  expect_true(inherits(GBR_USA1, 'ggplot'))

# label the lines
  ISOll <- data.frame(x=c(1500, 1800), y=c(2.5, 1.7), 
                      label=c('GBR', 'USA') )
  GBR_USA2 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                   labels=ISOll)  
  expect_true(inherits(GBR_USA2, 'ggplot'))
# vlines 
  Vlines = c(1849, 1929, 1933, 1939, 1945)
  GBR_USA3 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                   vlines=Vlines, labels=ISOll)  
  expect_true(inherits(GBR_USA3, 'ggplot'))
#      #  
#  expect_in('ggplot', class(GBR_USA))
#  GBR_USA <- plotMaddison(c('GBR', 'USA'))
})
