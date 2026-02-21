test_that("ggplotPath2", {
  expect_error(ggplotPath2())
  # matrix examples 
  Mat <- cbind(lvl=1:5, vel=rep(1:2, length=5), acc=sin(1:5))
  Mat1 <- Mat
  rownames(Mat1) <- 1951:1955
  Mat2 <- as.data.frame(cbind(Mat, year=1951:1955))
  
  # mts example 
  MTS <- ts(Mat, 1951)

  # Do  
  Matp <- ggplotPath2(Mat)
  expect_equal(class(Matp), c("egg", "gtable", "gTree", "grob", "gDesc" ))
  Matp1 <- ggplotPath2(Mat, object2=sqrt(1:5))
  expect_equal(class(Matp1), c("egg", "gtable", "gTree", "grob", "gDesc" ))
  Matp2 <- ggplotPath2(Mat, object2=2*Mat[, 2:1])
  expect_equal(class(Matp2), c("egg", "gtable", "gTree", "grob", "gDesc" ))
  
  Mat1p <- ggplotPath2(Mat1)
  expect_equal(class(Mat1p), c("egg", "gtable", "gTree", "grob", "gDesc" ))
  
  Mat2p <- ggplotPath2(Mat2[, 1:3], Time=Mat2[, 'year']) 
  expect_equal(class(Mat2p), c("egg", "gtable", "gTree", "grob", "gDesc" ))
  
  MTSp <- ggplotPath2(MTS)
  expect_equal(class(MTSp), c("egg", "gtable", "gTree", "grob", "gDesc" ))
  
  MTSep <- ggplotPath2(MTS, logy=c('', 'log', 'exp_log'))
  expect_equal(class(MTSep), c("egg", "gtable", "gTree", "grob", "gDesc" ))

  # list example 
  List2 <- list(
      level=list('year', 'lvl', logy=''), 
      slope=list('year', 'vel', logy='log'), 
      accel=list('year', 'acc', logy='exp_log'))
# ggplotPath2.list(List2, data=Mat2)
  Mat2l <- ggplotPath2(List2, data=Mat2)
  expect_equal(class(Mat2l), c("egg", "gtable", "gTree", "grob", "gDesc" ))
  
  # State space / Kalman filtering model for GBR
  GBR <- subset(MaddisonData, (ISO=='GBR') & !is.na(gdppc))
  
  # model example 
#  GBRgrowthMdl <- growthModel(.04, GBR$gdppc, Time=GBR$year)
  growthFormula <- (log(gdppc)~ -1 + SSMbespoke(growthModel(.04, GBR$gdppc) )) 
  library(KFAS)
  GBR2m <-SSModel(growthFormula, GBR, H=matrix(NA) )
  # This call first gives a warning and then an error. 
  expect_warning(expect_error(GBR2mp <- ggplotPath2(GBR2m$a)))
  expect_error(expect_warning(GBR2mp <- ggplotPath2(GBR2m$a)))
  
  GBRgrowthFit1 <- fitSSM(GBR2m, inits=-6, method = "BFGS", 
                          updatefn = growthUpdateFn)
  # NOTE: This call ignores Time 
  GBRgrowthFit1t <- fitSSM(GBR2m, inits=-6, method = "BFGS", 
                          updatefn = growthUpdateFn, Time=GBR$year)
  # *******
  # NOTE: This call currently also ignores Time: MUST BE FIXED 
  expect_identical(GBRgrowthFit1, GBRgrowthFit1t)
  
  expect_error(ggplotPath2(GBRgrowthFit1))
  
  #KFS example 
  GBR_KFS <- KFAS::KFS(GBRgrowthFit1$model)
  GBR_KFSt <- KFAS::KFS(GBRgrowthFit1t$model)
  GBR_KFSp0 <- ggplotPath2(GBR_KFS, Time=GBR$year)
  GBR_KFSp <- ggplotPath2(GBR_KFS$a)
  GBR_KFStp <- ggplotPath2(GBR_KFSt$a)

  expect_identical(class(GBR_KFSp), 
                   c("egg", "gtable", "gTree", "grob", "gDesc") )
                   
# label the lines
  ISOll <- data.frame(x=c(1649, 1800, 1810, 1985), y=c(5, 8, 6, 6), 
                      label=c('Civil War', 'Napoleon', 'Empire', 'Keynes'), 
                      srt=c(90, 85, 0, 0), 
                      col=c('red', 'green'), 
                      size=c(3, 4, 5, 3))
  expect_error(ggplotPath2(GBR_KFS$a, labels=ISOll))
  
  ISOll1 <- cbind(ISOll, component=1)
  expect_warning(
    expect_warning(
    GBR_KFSp1 <- ggplotPath2(GBR_KFS$a, labels=ISOll1)
    )
  )
  ISOll2 <- cbind(ISOll, component=c(1, 1, 2, 2))

  expect_warning(
    expect_warning(
      GBR_KFSp2 <- ggplotPath2(GBR_KFS, labels=ISOll2)
    )
  )
  
  expect_identical(class(GBR_KFSp2), 
                   c("egg", "gtable", "gTree", "grob", "gDesc") )
  
  # vlines 
  zero <- 0
  attr(zero, 'color') <- 'red'
  attr(zero, 'lty') <- 'dashed'
  Hlines1 <- list(c(1,3, 10, 30), zero) 
  Vlines <- c(1649, 1929, 1933, 1945)
  
  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
            GBR_KFSp3 <- ggplotPath2(GBR_KFS, labels=ISOll2, hlines=Hlines1, 
                                     vlines=Vlines)
        )
      )
    )
  )
  
  expect_identical(class(GBR_KFSp3), 
                   c("egg", "gtable", "gTree", "grob", "gDesc") )
  
  GBR_KFSp4 <- ggplotPath2(GBR_KFS, Time=GBR$year, object2=GBR$gdppc, 
                           labels=ISOll2, hlines=Hlines1, vlines=Vlines)
  expect_identical(class(GBR_KFSp4), 
                   c("egg", "gtable", "gTree", "grob", "gDesc") )
  GBR_KFSp4a <- ggplotPath2(GBR_KFS, Time=GBR$year, object2=GBR$gdppc, 
                           scaley=c(1100, .009), 
                           labels=ISOll2, hlines=Hlines1, vlines=Vlines)
  expect_identical(class(GBR_KFSp4), 
                   c("egg", "gtable", "gTree", "grob", "gDesc") )
  

})
