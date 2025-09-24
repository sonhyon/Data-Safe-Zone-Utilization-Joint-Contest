library(dplyr) ; library(stringr) ; library(tidyr) ; library(factoextra) ; library(tidyr)

getwd()
#───────────────────────────────────────────────────────────────────────────────
#*[노선별 교통사고_2018]
Traffic_Accident <- read.csv("./데이터/군집분석_도로교통공단_고속도로 노선별 교통사고수_2018.csv", fileEncoding = "CP949")

Traffic_Accident <- Traffic_Accident %>%
  rename(노선명 = 노선)
Traffic_Accident$노선명 <- gsub(" ", "", Traffic_Accident$노선명)
head(Traffic_Accident)

save(Traffic_Accident, file = "./r_data/Traffic_Accident.rdata")

#───────────────────────────────────────────────────────────────────────────────
#*[노선별 교통량_2005~2024]
traffic_volume <- read.csv("./데이터/군집분석_한국도로공사_노선별 교통량_(2005~2024).csv", fileEncoding = "CP949")
head(traffic_volume)

traffic_volume_2018 <- traffic_volume %>%
  select(구분, 차종, X2018년) %>%
  group_by(구분) %>%
  summarise(total_volume = sum(X2018년)) %>%
  filter(total_volume != 0) %>%
  rename(노선명 = 구분)

traffic_volume_2018$노선명 <- gsub(" ", "", traffic_volume_2018$노선명)
head(traffic_volume_2018)

save(traffic_volume_2018, file = "./r_data/traffic_volume_2018.rdata")
#───────────────────────────────────────────────────────────────────────────────
#*[노선별 교통사고 원인_2022~2024]
cause_of_traffic_accident <- read.csv("./데이터/군집분석_한국도로공사_고속도로 교통사고 원인_(2022~2024).csv", fileEncoding = "CP949")
head(cause_of_traffic_accident)

cause_of_traffic_accident_2022 <- cause_of_traffic_accident %>%
  filter(원인 != '기타', 사고년도 == '2022년') %>%
  group_by(노선명, 원인) %>%
  summarise(cnt = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = 원인,
    values_from = cnt,
    values_fill = list(cnt = 0)   # NA 대신 0으로 채움
  )

cause_of_traffic_accident_2022$노선명 <- gsub(" ", "", cause_of_traffic_accident_2022$노선명)
head(cause_of_traffic_accident_2022)

save(cause_of_traffic_accident_2022, file = "./r_data/cause_of_traffic_accident_2022.rdata")

#───────────────────────────────────────────────────────────────────────────────
#*[노선별 졸음운전_2022~2024]
drowsy_driving <- read.csv("./데이터/군집분석_한국도로공사_졸음운전 사고통계_(2022~2024).csv", fileEncoding = "CP949")
head(drowsy_driving)

drowsy_driving_2022 <- drowsy_driving %>%
  filter(사고연도 == '2022년') %>%
  group_by(노선명) %>%
  summarise(방향 = n())

drowsy_driving_2022$노선명 <- gsub(" ", "", drowsy_driving_2022$노선명)
head(drowsy_driving_2022)

save(drowsy_driving_2022, file = "./r_data/drowsy_driving_2022.rdata")
