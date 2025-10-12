test_that("MaddisonData", {
# Get the country for a countrycode (ISO): 
  GBR <- MaddisonData[MaddisonData$ISO=='GBR', ]
  expect_equal(ncol(GBR), 4)
  expect_gt(nrow(GBR), 206)
})
