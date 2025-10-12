test_that("MadDateRanges", {
  expect_equal(MadDateRanges(
    c('1', '700 – 1500', '1820, 1870, 1913, 1950')), 
    data.frame(
      yearBegin=c(1,  700, 1820, 1870, 1913, 1950), 
      yearEnd  =c(1, 1500, 1820, 1870, 1913, 1950), 
      sourceNum=c(1, 2, rep(3, 4)))
    )
})
