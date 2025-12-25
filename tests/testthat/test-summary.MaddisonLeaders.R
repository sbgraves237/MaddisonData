test_that("summary.MaddisonLeaders", {
  Leaders0 <- MaddisonLeaders() # max GDPpc for each year. 
  sLdrs <- summary(Leaders0) 
  expect_in('data.frame', class(Leaders0))
  dy1 <- (sLdrs$yearEnd - sLdrs$yearBegin)
  expect_gte(min(dy1), 0)
#
  expect_gte(min(sLdrs$n), 1)
  expect_gt(min(sLdrs$p), 0)
  expect_lte(max(sLdrs$p), 1)
})
