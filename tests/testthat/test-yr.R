test_that("yr", {
  Jan2_24_25 <- c('2024-01-02', '2025-01-02')
  J2yr <- yr(Jan2_24_25)
  J2y_ <- (c(2024, 2025) + (1/c(366,365)))
  expect_identical(J2yr, J2y_)
  J2y <- yr(as.POSIXct(Jan2_24_25))
  expect_identical(J2yr, J2y)
})
