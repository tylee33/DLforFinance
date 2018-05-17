# install.packages(c('devtools','curl'))
library('devtools')
devtools::install_github("opop4615/sungkiquant")

require(sungkiquant)
Sys.setlocale(category = "LC_CTYPE", locale = "Ko_KR.UTF-8")
get_code("티웨이홀딩스")
get_code("대한항공")

library(xts)
library(quantmod)

tway <- getSymbols("004870.KS", auto.assign = F, from = '2016-01-01')
koreanair <- getSymbols("003490.KS", auto.assign = F, from = '2016-01-01')

head(tway)
tail(tway)
head(koreanair)
tail(koreanair)

colnames(tway) <- c("op","hi","lo","close","vol","adj")
colnames(koreanair) <- c("op","hi","lo","close","vol","adj")

layout(matrix(c(1,2), 1, 2, byrow = TRUE))

plot(tway[,4],col='red', main='tway Price')
plot(koreanair[,4],col='blue', main='koreanair price')

summary(tway[,4])
summary(koreanair[,4])

layout(matrix(c(1,2,3), 3, 1, byrow = TRUE))
plot(dailyReturn(tway[,4]),col='red', main='tway dailyreturn')
plot(weeklyReturn(tway[,4]),col='blue', main='tway weeklyreturn')
plot(monthlyReturn(tway[,4]),col='green', main='tway monthlyreturn')

summary(dailyReturn(tway[,4]))
summary(weeklyReturn(tway[,4]))
summary(monthlyReturn(tway[,4]))

layout(matrix(c(1,2,3), 3, 1, byrow = TRUE ))
plot(dailyReturn(koreanair[,4]),col='red', main ='koreanair dailyreturn')
plot(weeklyReturn(koreanair[,4]),col='blue', main = 'koreanair weeklyreturn')
plot(monthlyReturn(koreanair[,4]),col='green', main = 'koreanair monthlyreturn')

summary(dailyReturn(koreanair[,4]))
summary(weeklyReturn(koreanair[,4]))
summary(monthlyReturn(koreanair[,4]))