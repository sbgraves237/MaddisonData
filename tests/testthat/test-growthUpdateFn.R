test_that("growthModel", {
  GBR <- subset(MaddisonData, (ISO=='GBR') & !is.na(gdppc))
  growthMdl0 <- growthModel(.1, GBR$gdppc)
  growthMdl0v <- growthUpdateFn(.1, growthMdl0)
  growthMdl1 <- growthModel(.1, GBR$gdppc, Time=GBR$year)
  growthMdl1v <- growthUpdateFn(.1, growthMdl1, Time=GBR$year)
  growthMdl1v3 <- growthUpdateFn(1:3, growthMdl1, Time=GBR$year)
  
  # check growthMdl1$state_names 
  expect_identical(growthMdl0v$state_names, growthMdl0$state_names)
  expect_identical(growthMdl1v$state_names, growthMdl1$state_names)
  expect_identical(growthMdl1v3$state_names, growthMdl1$state_names)
  # check H
  expect_identical(growthMdl0v$H, array(exp(.1), c(1, 1, 1)))
  expect_identical(growthMdl1v$H, array(exp(.1), c(1, 1, 1)))
  expect_identical(growthMdl1v3$H, array(exp(1), c(1, 1, 1)))
  # check Q
  Tm <- GBR$year
  N <- length(Tm)
  dT_ <- diff(Tm)
  dT <- c(dT_[1], dT_)
  v1 <- exp(.1)
  v23 <- exp(2:3)
  Q1 <- array(0, c(2, 2, length(Tm)))
  dimnames(Q1) <- list(growthMdl1$state_names, growthMdl1$state_names, 1:N)
  Q1[1, 1, ] <- v1
  Q1[2, 2, ] <- v1
  expect_identical(growthMdl0v$Q, Q1)
#   
  Q1t <- Q1 
  dimnames(Q1t) <- list(growthMdl1$state_names, growthMdl1$state_names, Tm)
  Q23 <- Q1t  
  Q1t[1, 1, ] <- (v1*dT + (dT*(dT-1)*(2*dT-1)/6)*v1)
  Q1t[2, 2, ] <- v1*dT
  Q123t <- choose(dT, 2)*v1 
  Q1t[1, 2, ] <- Q123t
  Q1t[2, 1, ] <- Q123t
  expect_identical(growthMdl1v$Q, Q1t)
#  
  Q23[1, 1, ] <- (v23[1]* dT + (dT*(dT-1)*(2*dT-1)/6)*v23[2])
  Q23[2, 2, ] <- (dT*v23[2]) 
  Q123 <- (choose(dT, 2)* v23[2])
  Q23[1, 2, ] <- Q123
  Q23[2, 1, ] <- Q123
  expect_identical(growthMdl1v3$Q, Q23)
#
  growthFmla <- (log(gdppc)~ -1 + 
                      KFAS::SSMbespoke(growthModel(.1, GBR$gdppc) )) 
  growthFml <- (log(gdppc)~ -1 + SSMbespoke(growthModel(.1, GBR$gdppc) )) 
  library(KFAS)
  # The following should NOT give an error, but it does. 
  expect_error(SSModel(growthFmla, GBR, H=matrix(NA) ))
# Error in model.frame.default(formula = growthFmla, data = GBR, na.action = na.pass) : 
#   invalid type (list) for variable 'KFAS::SSMbespoke(growthModel(0.1, GBR$gdppc))'
  growthSSmdl <-SSModel(growthFml, GBR, H=matrix(NA) )
  growthSSmdl1 <- growthUpdateFn(log(.01), growthSSmdl)
      
  expect_equal(growthSSmdl[-c(3, 6)], growthSSmdl1[-c(3, 6, 15)])
  expect_true(is.na(growthSSmdl$H))
  expect_true(all(abs(growthSSmdl$Q - growthSSmdl1$Q)<.Machine$double.eps))
# test for expected errors
  expect_error(growthUpdateFn())
  expect_error(growthUpdateFn(1))
  expect_error(growthUpdateFn(1:4, growthMdl1))
  expect_error(growthUpdateFn(1:3, list(a=1)))
})
