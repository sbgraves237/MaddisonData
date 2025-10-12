test_that("MaddisonCountries", {
# Get the country for a countrycode
  expect_contains(
    subset(MaddisonCountries, ISO=='GBR', country), 
    'United Kingdom'
  )
# Find Yugoslavia 
  YUG0 <- data.frame(ISO='YUG', 
                     country='Former Yugoslavia', 
                     region = 'Eastern Europe')
  expect_contains(YUG0, 
        subset(MaddisonCountries, grepl('Yugo', country), 1:3)
  ) 
# number of countries by region 
  expect_equal(8, length(table(MaddisonCountries$region)))
# What are "Western Offshoots"? 
  expect_equal(c(4, 2), dim(subset(MaddisonCountries, 
              grepl('Of', region), c(country, ISO))))
})
