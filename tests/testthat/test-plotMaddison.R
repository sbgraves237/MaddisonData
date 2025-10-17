test_that("plotMaddison", {
  expect_error(plotMaddison())
  expect_error(plotMaddison(c('GBR', 'x')))
  expect_error(plotMaddison(c('GBR', 'x'), character(0)))
  expect_error(plotMaddison('GBR', 'illegal y'))
  expect_error(plotMaddison('GBR', c('long', ' y')) )
  #  
  GBR_USA <- plotMaddison(c('GBR', 'USA'))
  expect_in('ggplot', class(GBR_USA))
})
