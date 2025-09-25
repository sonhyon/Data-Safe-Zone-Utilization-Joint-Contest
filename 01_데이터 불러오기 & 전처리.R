# 필요한 패키지
library(dplyr) ; library(rpart) ; library(rpart.plot) ; library(purrr)

#[분류분석 데이터 불러오기]

#종속변수
load("./r_data/Traffic_Accident.rdata")

#독립변수
structure <- read.csv("./데이터/분류분석_고속도로 구조물 현황(2024년).csv", fileEncoding = "CP949") #구조물_2024
load("./r_data/cause_of_traffic_accident_2022.rdata")
load("./r_data/traffic_wide.rdata")
Average_yearly_Traffic <- read.csv("./데이터/분류분석_한국도로공사_연평균일교통량_(2019~2024).csv", fileEncoding = "CP949") #연평균 교통량
load("./r_data/drowsy_driving_2022.rdata")

#───────────────────────────────────────────────────────────────────────────────
#[데이터 전처리]
rest_area_cnt <- rest_area %>%
  group_by(노선) %>%
  summarise(졸음쉼터수 = n()) %>%
  rename(노선명 = 노선)
head(rest_area_cnt)

structure <- structure %>%
  rename(노선명 = 노선)

head(Traffic_Accident) #교통사고_2018

head(structure) #구조물_2024
head(cause_of_traffic_accident_2022) #교통사고 원인_2022
head(traffic_wide) #교통량_2018
head(rest_area_cnt) #졸음쉼터
head(drowsy_driving_2022) #졸음운전_2022~2024

df <- Traffic_Accident %>%
  full_join(structure, by = "노선명") %>%
  full_join(cause_of_traffic_accident_2022, by = "노선명") %>%
  full_join(traffic_wide, by = "노선명") %>%
  full_join(rest_area_cnt, by = "노선명") %>%
  full_join(drowsy_driving_2022, by = "노선명")
head(df)

df_sel <- df %>%
  mutate(전체사고수 = 사고건수 + 사망자수 + 부상자수) %>%
  select(전체사고수, 소형차, 중형차, 대형차, 졸음운전사고수,주시태만, 졸음쉼터수)
print(df_sel)

df_clean <- df_sel[rowSums(df_sel == 0) == 0, ]
df_clean <- na.omit(df_clean)
print(df_clean)

#───────────────────────────────────────────────────────────────────────────────


library(rpart)
library(rpart.plot)

# 로그 변환 (0일 경우 문제 없도록 +1)
df_clean_log <- df_clean
df_clean_log$소형차 <- log(df_clean_log$소형차 + 1)
df_clean_log$중형차 <- log(df_clean_log$중형차 + 1)
df_clean_log$대형차 <- log(df_clean_log$대형차 + 1)
df_clean_log$졸음운전사고수 <- log(df_clean_log$졸음운전사고수 + 1)
df_clean_log$주시태만 <- log(df_clean_log$주시태만 + 1)
df_clean_log$졸음쉼터수 <- log(df_clean_log$졸음쉼터수 + 1)
df_clean_log$전체사고수 <- log(df_clean_log$전체사고수 + 1)

# 트리 모델 생성 (회귀)
tree_model <- rpart(전체사고수 ~ 소형차 + 중형차 + 대형차 + 졸음운전사고수 + 주시태만 + 졸음쉼터수,
                    data = df_clean_log,
                    method = "anova",
                    control = rpart.control(minsplit = 2, cp = 0.001)) # 분할 기준 완화

# 트리 시각화
rpart.plot(tree_model, type = 3, extra = 101, fallen.leaves = TRUE)
