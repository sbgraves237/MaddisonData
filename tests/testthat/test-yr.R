test_that("yr", {
  Jan2_24_25 <- c('2024-01-02', '2025-01-02')
  J2yr <- yr(Jan2_24_25)
  expect_true(inherits(J2yr,"numeric"))
  J2y <- yr(as.POSIXct(Jan2_24_25))
  expect_equal(J2yr, J2y)
})
