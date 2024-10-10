options(encoding="UTF-8")  # 한글 인코딩
library(KoNLP)             # 한글 자연어 분석
library("ggplot2")         # 시각화
library(dplyr)             # 전처리
library(kormaps2014)       # 대한민국 지도 데이터
library(ggiraphExtra)
korean_sample <- read.csv("한국환경공단_전국 전기차 충전소 현황_20221110.csv", fileEncoding = 'euc-kr')
head(korean_sample, 10)
class(korean_sample$시도)
table(korean_sample$시도)
class(korean_sample$급속충전량)
table(korean_sample$급속충전량)
city <- korean_sample %>%
  select(시도, 급속충전량) %>%
  filter(급속충전량 != "") %>%
  group_by(시도, 급속충전량) %>%
  summarise(counted =n())
city <- rename(city, name = 시도)
city
areacode
testing_100alone <- city %>%
  filter(급속충전량 == "급속(100kW단독)")
last_100alone <- left_join(areacode, testing_100alone, by = )
ggChoropleth(data = last_100alone, #지도에 표현할 데이터
             aes(fill = counted,   # 색깔로 표현할 변수
                 map_id = code,    # 지역 기준 변수
                 tooltip = name),  #지도 위에 표시할 지역명
             map = kormap1,        # 지도 데이터
             interactive = T)      # 인터랙티브
testing_100both <- city %>%
  filter(급속충전량 == "급속(100kW동시)")
last_100both <- left_join(areacode, testing_100both, by = )
ggChoropleth(data = last_100both,
             aes(fill = counted,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
testing_100multy <- city %>%
  filter(급속충전량 == "급속(100kW멀티)")
last_100multy <- left_join(areacode, testing_100multy, by = )
ggChoropleth(data = last_100multy,
             aes(fill = counted,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
testing_200both <- city %>%
  filter(급속충전량 == "급속(200kW동시)")
last_200both <- left_join(areacode, testing_200both, by = )
ggChoropleth(data = last_200both,
             aes(fill = counted,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
testing_400both <- city %>%
  filter(급속충전량 == "급속(400kW동시)")
last_400both <- left_join(areacode, testing_400both, by = )
ggChoropleth(data = last_400both,
             aes(fill = counted,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
testing_50 <- city %>%
  filter(급속충전량 == "급속(50kW)")
last_50 <- left_join(areacode, testing_50, by = )
ggChoropleth(data = last_50,
             aes(fill = counted,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
testing_350alone <- city %>%
  filter(급속충전량 == "초급속(350kW단독)")
last_350alone <- left_join(areacode, testing_350alone, by = )
ggChoropleth(data = last_350alone,
             aes(fill = counted,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
