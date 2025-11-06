test_that("MaddisonLeaders", {
  Leaders0 <- MaddisonLeaders() # max GDPpc for each year. 
  expect_in('data.frame', class(Leaders0))
  expect_equal(colnames(Leaders0), c(
    'yearBegin', 'yearEnd', 'gdppc0', 'gdppc1', 'ISO', 'dy0','dy1'))
  L0NA <- sapply(Leaders0, function(x)sum(is.na(x)))
  expect_equal(L0NA, c(yearBegin=0, yearEnd=0, gdppc0=0, gdppc1=0, ISO=0, 
                       dy0=0, dy1=1))
  expect_gt(length(table(Leaders0$ISO)), 1)
# attr  
  LeaderByYear0 <- attr(Leaders0, 'LeaderByYear')
  expect_in('data.frame', class(LeaderByYear0))
  expect_equal(colnames(LeaderByYear0), c('year', 'maxgdppc', 'ISO'))
  LbY0NA <- sapply(LeaderByYear0, function(x)sum(is.na(x)))
  expect_equal(LbY0NA, c(year=0, maxgdppc=0, ISO=0))
  expect_gt(length(table(LeaderByYear0$ISO)), 1)
  expect_equal(length(table(Leaders0$ISO)), 
               length(table(LeaderByYear0$ISO)))
# Presumed technology leaders 
#   without obvious commodity leaders with narrow economies 
  Leaders1 <- MaddisonLeaders(c('ARE', 'KWT', 'QAT')) 
  expect_in('data.frame', class(Leaders1))
  expect_equal(colnames(Leaders1), c(
    'yearBegin', 'yearEnd', 'gdppc0', 'gdppc1', 'ISO', 'dy0', 'dy1'))
  L1NA <- sapply(Leaders1, function(x)sum(is.na(x)))
  expect_equal(L1NA, c(yearBegin=0, yearEnd=0, gdppc0=0, gdppc1=0, ISO=0, 
                       dy0=0, dy1=1))
  expect_gt(length(table(Leaders1$ISO)), 1)
# attr  
  LeaderByYear1 <- attr(Leaders1, 'LeaderByYear')
  expect_in('data.frame', class(LeaderByYear1))
  expect_equal(colnames(LeaderByYear1), c('year', 'maxgdppc', 'ISO'))
  LbY1NA <- sapply(LeaderByYear1, function(x)sum(is.na(x)))
  expect_equal(LbY1NA, c(year=0, maxgdppc=0, ISO=0))
  expect_gt(length(table(LeaderByYear1$ISO)), 1)
  expect_equal(length(table(Leaders1$ISO)), 
               length(table(LeaderByYear1$ISO)))
# since 16MadDat1600 <- subset(MaddisonData, year>1600)
  MadDat1600 <- subset(MaddisonData, year>1600)
  Leaders1600 <- MaddisonLeaders(c('ARE', 'KWT', 'QAT'), data=MadDat1600)
  expect_gt(min(Leaders1600$yearBegin), 1600)
})
