test_that("growthModel", {
  GBR <- subset(MaddisonData, (ISO=='GBR') & !is.na(gdppc))
  growthMdl1 <- growthModel(.1, GBR$gdppc, Time=GBR$year)
  GBRgdppc1 <- with(GBR[-1, ], ts(gdppc, year[1]))
  growthMdl2 <- growthModel(c(.1, .2), GBRgdppc1)
  growthMdl0 <- growthModel(.1, GBR$gdppc, a1=c(10, 1), Log=FALSE, 
                     stateNames=c('lvl', 'vel'))
# check growthMdl1$state_names 
  expect_identical(growthMdl1$state_names, c('level', 'growthRate'))
# growthMdl1 and growthMdl2 share some components  
  expect_identical(growthMdl1[c(1:4, 6, 11:17)], growthMdl2[c(1:4, 6, 11:17)])
  expect_equal(growthMdl1[[5]], growthMdl2[[5]]+1)
  expect_identical(growthMdl1[[6]][,,-(1:2)], growthMdl2[[6]][,,-1])
  expect_identical(growthMdl1[[7]][,,-(1:2)], growthMdl2[[7]][,,-1])
  expect_identical(growthMdl1[[8]][,,-(1:2)], growthMdl2[[8]][,,-1])
  Q1.2 <- growthMdl1[[9]][,,-(1:2)]
  Q2.1 <- growthMdl2[[9]][,,-1]
  Q2.1[2,2, ] <- .01
  expect_equal(Q1.2,Q2.1)
  expect_equal(growthMdl2[[9]][2,2,-1], rep(.04, 770))
#   
  expect_equal(as.numeric(growthMdl1[[10]]), 
                   with(GBR, log(c(gdppc[1], gdppc[2]/gdppc[1]))))
# growthMdl1 and growthMdl0 share some components  
  expect_identical(growthMdl1[c(1:5, 6, 11:16)], growthMdl0[c(1:5, 6, 11:16)])
  expect_identical(growthMdl1[[6]][,,-1], growthMdl0[[6]][,,-1])
  expect_identical(growthMdl1[[7]][,,-(1:2)], growthMdl0[[7]][,,-(1:2)])
  expect_identical(growthMdl1[[8]][,,-(1:2)], growthMdl0[[8]][,,-(1:2)])
  expect_identical(growthMdl1[[9]][,,-(1:2)], growthMdl0[[9]][,,-(1:2)])
  #   
  expect_equal(as.numeric(growthMdl0[[10]]), c(10, 1))
# test for expected errors
  expect_error(growthModel(.1, 1:4, 1:4)) #length(a1) != 2
  expect_error(growthModel(.1, 1:4, 1:4)) #length(Time) != length(y)
})
