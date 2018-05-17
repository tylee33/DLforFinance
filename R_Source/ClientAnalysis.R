# RFM(Recency, Frequency, Monetary)은 고객의 가치(매출???)를 다음 세가지 측도로 평가하고, 이 모형을 바탕으로 연관성과 더불어 필요하면 가설을 도출할 수도 있다.
# R ecency- 거래 최근성: 고객이 얼마나 최근에 구입했는가?
# F requency- 거래빈도: 고객이 얼마나 빈번하게 우리 상품을 구입했나?
# M onetary- 거래규모: 고객이 구입했던 총 금액은 어느 정도인가?
# V고객가치=f(거래최근성,거래빈도,거래규모)=β0+β1×거래최근성+β2×거래빈도+β3×거래규모+ϵ

# 0. 환경설정 -----------
#library(tidyverse)
#library(DT)
#library(lubridate)
#library(skimr)
#library(extrafont)
#library(ggpubr)
#loadfonts()
#library(knitr)
#library(kableExtra)


# 1. 데이터 가져오기 -----------
## 1.1. 데이터 다운로드
download.file(url="http://www.brucehardie.com/datasets/CDNOW_master.zip", destfile = "data/CDNOW_master.zip")
unzip("data/CDNOW_master.zip", exdir ="data/CDNOW")

## 1.2. 데이터 가져오기
#library(tidyverse)
cdnow_dat <- read_fwf("data/CDNOW/CDNOW_master.txt", fwf_cols(cid = 6, pdate = 9, numCDs = 3, amount=8))
#library(DT)
DT::datatable(cdnow_dat)

# 2. 데이터 정제 및 RFM -----------

cdnow_df <- cdnow_dat %>% 
  mutate(pdate = parse_date(pdate, format = "%Y%m%d")) %>% 
  select(-numCDs)

#library(skimr)
skim(cdnow_df)

#library(lubridate)
cdnow_rfm_df <- cdnow_df %>% 
  mutate(purchase_gap = difftime(ymd("1998-07-01"), pdate, units = "day")) %>% 
  group_by(cid) %>% 
  summarize(recency = min(purchase_gap),
            frequency = n(),
            monetary = sum(amount))

DT::datatable(cdnow_rfm_df) %>% 
  DT::formatCurrency(c("monetary"), currency="$ ", interval = 3, digits = 0)

#library(ggpubr)
cdnow_rfm_df %>% 
  gather(rfm_key, rfm_value, -cid) %>%
  ggplot(aes(x=rfm_value, color=rfm_key, fill= rfm_key)) +
  geom_histogram() +
  facet_wrap(~rfm_key, scale="free", nrow=3) +
  theme_pubclean(base_family = "NanumGothic") +
  theme(legend.position = "none") +
  labs(x="", y="") +
  scale_y_continuous(labels = scales::comma)

# 3. RFM 매트릭스 -----------

cdnow_rfm_df <- cdnow_rfm_df %>% 
  mutate(R = factor(ntile(recency, 4)),
         F = factor(ntile(frequency, 4)),
         M = factor(ntile(monetary, 4)))

cdnow_rfm_df %>% group_by(R, F) %>% 
  summarise(M_sum = sum(monetary)) %>% 
  DT::datatable() %>% 
  formatCurrency('M_sum', currency="", digits =0, interval =3) %>% 
  formatStyle(
    c('R'),
    target = 'row',
    backgroundColor = styleEqual(c(1,2,3,4), c('white', 'white', 'white', 'yellow'))
  )

# 4. 시각화 -----------
## 4.1. 단변량
cdnow_rfm_df %>% 
  ggplot(aes(y=log10(monetary), x=F)) +
  geom_boxplot()

cdnow_rfm_df %>% 
  ggplot(aes(y=log10(monetary), x=R)) +
  geom_boxplot()

## 4.2. 다변량

cdnow_rfm_df %>% 
  ggplot(aes(y=log10(monetary), x=R, fill=F)) +
  geom_boxplot()