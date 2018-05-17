# File name : 1.CollectData.R
# 한 종목의 데이터를 가져온다 
getData <- function(x, from) {
  if (missing(from))
    stock <- getSymbols(paste(x, ".KS", sep = ""), auto.assign = FALSE)
  else
    stock <- getSymbols(paste(x, ".KS", sep = ""), from=from, auto.assign = FALSE)
  # 모든 분석은 수정 주가 (adjusted)를 이용한다
  stock <- adjustOHLC(stock, use.Adjusted=TRUE)
  # 거래량이 0 인 데이터는 제외한다 (공,휴일) stock <- stock[Vo(stock) > 0]
  # colume 이름을 바꾼다
  colnames(stock) <- c("open", "high", "low", "close", "volume", "adjusted")
  getData <- stock 
}
# 종합주가지수를 읽어온다 
getKospi <- function(from) {
  if (missing(from))
    stock <- getSymbols('^KS11', auto.assign = FALSE)
  else
    stock <- getSymbols('^KS11', from=from, auto.assign = FALSE)
  # 수정주가로 바꾼 후 리턴
  stock <- adjustOHLC(stock, use.Adjusted=TRUE)
  # colume 이름을 바꾼다
  colnames(stock) <- c("open", "high", "low", "close", "volume", "adjusted")
  getKospi <- stock 
}