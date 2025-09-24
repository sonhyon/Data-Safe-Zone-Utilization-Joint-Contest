library(dplyr) ; library(tidyr) ; library(ggplot2)

Traffic_Accident_year <- read.csv("./데이터/기술통계_한국도로공사_교통사고통계_20241231.csv", fileEncoding = "CP949")
head(Traffic_Accident_year)

traffic_recent <- Traffic_Accident_year %>%
  filter(연도 >= 2015 & 연도 <= 2024)

traffic_long <- traffic_recent %>%
  pivot_longer(
    cols = c(사고, 사망, 부상),
    names_to = "구분",
    values_to = "건수"
  )

ggplot(traffic_long, aes(x = 연도, y = 건수, color = 구분)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(title = "2015~2024 연도별 교통사고 현황",
       x = "연도",
       y = "건수",
       color = "구분") +
  theme_minimal(base_size = 14)
