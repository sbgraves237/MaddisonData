test_that("ggplotPath", {
  expect_error(ggplotPath())
  expect_error(ggpotPath(c('GBR', 'x')))
  expect_error(ggpotPath(c('GBR', 'x'), character(0)))
  expect_error(ggpotPath('GBR', 'illegal y'))
  expect_error(ggpotPath('GBR', c('long', ' y')) )

  GBR_USA <- subset(MaddisonData::MaddisonData, ISO %in% c('GBR', 'USA'))
  GBR_USA1 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, scaley=1000)
  expect_true(inherits(GBR_USA1, 'ggplot'))
  GBR_USA1a <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                          color=c('red', 'blue'))
  expect_true(inherits(GBR_USA1, 'ggplot'))
  
# label the lines
  ISOll <- data.frame(x=c(1500, 1800), y=c(2.5, 1.7), 
                      label=c('GBR', 'USA'), srt=c(0, 30),
                      col=c('red', 'blue'), size=c(2, 9))
  GBR_USA2 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                   labels=ISOll, fontsize = 20, color=c('red', 'blue'), 
                   linetype=c('dotted', 'dashed'))  
  expect_true(inherits(GBR_USA2, 'ggplot'))
# vlines 
  Hlines <- c(1,3, 10, 30)
  Vlines <- c(1649, 1929, 1933, 1945)
  ISOl3 <- data.frame(x=c(1500, 1800), y=c(2.5, 1.7), 
                      label=c('GBR', 'USA'), srt=c(0, 30),
                      col=c('red', 'green'))
  GBR_USA3 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                  ylab='GDP per capita (2011 PPP K$)', 
                  legend.position = NULL, hlines=Hlines, vlines=Vlines, 
                  labels=ISOl3, col=c('red', 'green'))  
  expect_true(inherits(GBR_USA3, 'ggplot'))
# test error detection 
  Hlinesec <- Hlines
  attr(Hlinesec, 'color') <- 1:11
  expect_error(ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                    ylab='GDP per capita (2011 PPP K$)', 
                    legend.position = NULL, hlines=Hlinesec, vlines=Vlines, 
                    labels=ISOl3, col=c('red', 'green')) ) 
  Hlineset <- Hlines
  attr(Hlineset, 'lty') <- 1:11
  expect_error(ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                    ylab='GDP per capita (2011 PPP K$)', 
                    legend.position = NULL, hlines=Hlineset, vlines=Vlines, 
                    labels=ISOl3, col=c('red', 'green')) ) 
  Vlinesec <- Vlines
  attr(Vlinesec, 'color') <- 1:11
  expect_error(ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                    ylab='GDP per capita (2011 PPP K$)', 
                    legend.position = NULL, hlines=Hlines, vlines=Vlinesec, 
                    labels=ISOl3, col=c('red', 'green')) ) 
  Vlineset <- Vlines
  attr(Vlineset, 'lty') <- 1:11
  expect_error(ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                    ylab='GDP per capita (2011 PPP K$)', 
                    legend.position = NULL, hlines=Hlines, vlines=Vlineset, 
                    labels=ISOl3, col=c('red', 'green')) ) 
# do.call(ggplotPath, ...) with 1 line
  list1 <- list(x='Time', y='lvl', 
                data=data.frame(Time=letters[1:4], lvl=sqrt(1:4)))
  expect_error(do.call(ggplotPath, list1))
# unique(unique(dat[, group, drop=TRUE]) == names(color))
#  GBR_USA2 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
#                         labels=ISOll, fontsize = 20, color=c('red', 'blue'), 
#                         linetype=c('dotted', 'dashed'))  
  GBR_USA2b <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                          labels=ISOll, fontsize = 20, 
                          color=c(GBR='red', USA='blue'), 
                          linetype=c(GBR='dotted', USA='dashed'))  
#  expect_equal(GBR_USA2b, GBR_USA2) #plots look the same but aren't internally
  GBR_USA2c <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                          labels=ISOll, fontsize = 20, 
                          color=c(USA='blue', GBR='red'), 
                          linetype=c(GBR='dotted', USA='dashed'))  
#  expect_equal(GBR_USA2b, GBR_USA2c) #plots look the same but aren't internally
  expect_error(ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                            color=c(gbr='red', usa='blue'), 
                            linetype=c(GBR='dotted', USA='dashed')) )  
  expect_error(ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                          color=c(GBR='red', USA='blue'), 
                          linetype=c(gbr='dotted', usa='dashed'))  )
})
