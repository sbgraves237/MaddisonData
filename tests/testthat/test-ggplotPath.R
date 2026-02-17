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
                      label=c('GBR', 'USA'), srt=c(0, 30),
                      col=c('red', 'green'), size=c(2, 9))
  GBR_USA2 <- ggplotPath('year', 'gdppc', 'ISO', GBR_USA, 1000, 
                   labels=ISOll, fontsize = 20, color=c('red', 'green'))  
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
# do.call(ggplotPath, ...) with 1 line
  list1 <- list(x='Time', y='lvl', 
                data=data.frame(Time=letters[1:4], lvl=sqrt(1:4)))
  expect_error(do.call(ggplotPath, list1))
})
