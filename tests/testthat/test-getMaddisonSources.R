test_that("getMaddisonSources", {
  MDPall <- getMaddisonSources() # all 
  MDPonly <- getMaddisonSources(plot=FALSE) # only MDP 
  GBR <- getMaddisonSources('GBR') # GBR 
# Display all
#  as.data.frame(GBR)
# Only MDP   
  MDPonly12 <- getMaddisonSources(
    names(MaddisonData::MaddisonSources)[1:12], FALSE)#only MDP 
  GBR_USAsince1500 <- getMaddisonSources(data.frame(ISO=c('GBR', 'USA'), 
                yearBegin=rep(1500, 2)) ) #GBR, USA since 1500 
  GBR_USA1500_1790 <- getMaddisonSources(data.frame(ISO=c('GBR', 'USA'), 
      yearBegin=rep(1500, 2), yearEnd=rep(1790, 2)) ) #GBR, USA 1500-1790 
# MDPall_
  MDP1 <- data.frame(ISO=character(3), 
    years=c('2008-', '1990-', '1, .., 2022'), 
    source=c(paste('GDP pc(2008-): Total Economy Database (TED) of the',
          'Conference Board for all countries included in TED', 
          '[https://www.conference-board.org/topics/total-economy-database].',
          'Otherwise UN national accounts statistics'), 
        paste('population(1990-):Total Economy Database (TED) of the', 
          'Conference Board for all countries included in TED', 
          '[https://www.conference-board.org/topics/total-economy-database].',
          'Otherwise UN national accounts statistics'), 
        paste('Jutta Bolt and Jan Luiten Van Zanden (2024) "Maddison style estimates', 
              'of the evolution of the world economy: A new 2023 update",', 
              'Journal of Economic Surveys, 1-41')
             ) )
  
  MDPall_ <- MDP1 
  for(iso in names(MaddisonData::MaddisonSources)){
    isoi <- cbind(ISO=iso, MaddisonData::MaddisonSources[[iso]])
    MDPall_ <- rbind(MDPall_, isoi)
  }
  expect_equal(MDPall, MDPall_)
# MDPonly   
  MDPonly_ <- data.frame(ISO='', years='1, .., 2022', 
    source=paste('Jutta Bolt and Jan Luiten Van Zanden (2024)', 
      '"Maddison style estimates of the evolution of the world economy:', 
      'A new 2023 update", Journal of Economic Surveys, 1-41') )
  expect_equal(MDPonly, MDPonly_)
# GBR
  GBR1 <- cbind(ISO='GBR', MaddisonData::MaddisonSources[['GBR']])
  GBR_ <- rbind(MDP1, GBR1)
  expect_equal(GBR, GBR_)
# MDPonly12
  expect_equal(MDPonly12, MDPonly_)
# GBR_USAsince1500
  GBRsince1500 <- GBR1[-1,]
  USA_ <- cbind(ISO='USA', MaddisonData::MaddisonSources[['USA']])
  GBR_USAsince1500_ <- rbind(MDP1, GBRsince1500, USA_)
  row.names(GBR_USAsince1500_) <- NULL
  expect_equal(GBR_USAsince1500, GBR_USAsince1500_) 
# GBR_USA1500_1790
  GBR_USA1500_1790_ <- rbind(MDP1, GBRsince1500, USA_[-3,])
  row.names(GBR_USA1500_1790_) <- NULL
  expect_equal(GBR_USA1500_1790, GBR_USA1500_1790_) 
# AUS 
  AUS <- getMaddisonSources('AUS') # AUS: no special sources for AUS. 
  expect_equal(AUS, MDPall[1:3, ])
})
