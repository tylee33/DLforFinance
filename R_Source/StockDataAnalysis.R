library(quantmod)
s <- getSymbols('005930.KS', auto.assign = FALSE)
colnames(s) <- c('open', 'high', 'low', 'close', 'volume', 'adjusted')
head(s, 3)
tail(s, 3)
s <- getSymbols('005930.KS', from='2010-01-01', auto.assign = FALSE)
colnames(s) <- c('open', 'high', 'low', 'close', 'volume', 'adjusted')
s <- s[Vo(s) > 0] 
s$rtn <- ROC(Cl(s)) # ?????? ?????? ????????? ??????
head(s, 3)
s <- na.omit(s)
head(s, 3)
drift <- mean(s$rtn) * 252 * 100
drift
volatility <- sd(s$rtn) * sqrt(252) * 100
volatility
plot(as.vector(s$close), type="l", col="blue")
plot(as.vector(s$rtn), type="l", col="red")
abline(h=mean(s$rtn))
plot(density(as.vector(s$rtn)), col="red")
abline(v = 0, col = "blue")
mean(s$rtn)
sd(s$rtn)
barplot(as.vector(s$volume['2016-09-15::']), col='red')
s$sma5 <- SMA(s$close, 5)
s$sma10 <- SMA(s$close, 10)
plot(as.vector(s$close['2016-07-01::']), type='l')
lines(as.vector(s$sma5['2016-07-01::']), col='red')
lines(as.vector(s$sma10['2016-07-01::']), col='blue')
chartSeries(s['2016-05-01::'], up.col='red', dn.col='blue', theme='white', name="Samsung")
addMACD() # CCS ????????? ?????? ??????
addSMA(10)
addSMA(30, col='blue')# 10???, 30??? ??????????????? ??????

chartSeries(to.weekly(s['2016-05-01::']), up.col='red', dn.col='blue', theme='white', name="Samsung")
addMACD() # CCS ????????? ?????? ??????
addBBands() # ????????? ?????? ??????
addSMA(10); addSMA(30, col='blue')# 10???, 30??? ??????????????? ?????? * ????????? to.monthly() ??????