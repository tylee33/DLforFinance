require(quantmod)
require(PerformanceAnalytics)

#get the data and fill out the MA
getSymbols('SPY', from='1950-01-01')
SPY$ma200 <- SMA(Cl(SPY), 200)
SPY$ma50 <- SMA(Cl(SPY), 50)

#lets look at it from 2000 to 2015
spy <- SPY['1950/2015']

#our baseline, unfiltered results
ret <- ROC(Cl(spy))

#our comparision, filtered result
ma_sig <- Lag(ifelse(SPY$ma50 > SPY$ma200, 1, 0))
ma_ret <- ROC(Cl(spy)) * ma_sig

golden<- cbind(ma_ret,ret)
colnames(golden) = c('GoldCross','Buy&Hold')

#Plot to visually see the actual moving averages
chartSeries(spy,
            type = 'line',
            name = 'Moving Average : Golden Cross',
            TA= c(addSMA(50, col = 'yellow'), addSMA(200)))

maxDrawdown(golden)
table.AnnualizedReturns(golden, Rf= 0.02/252)
charts.PerformanceSummary(golden, Rf = 0.02, main='Golden Cross',geometric=FALSE)