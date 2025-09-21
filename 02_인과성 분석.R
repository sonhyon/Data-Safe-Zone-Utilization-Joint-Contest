#*[종속변수]
head(Traffic_Accident) #교통사고_2018~2024
head(Traffic_Accident_cnt) #교통사고_2018~2024

#───────────────────────────────────────────────────────────────────────────────
#*[독립변수]
head(highway_structures_all) #2019~2021,2024
head(Highway_Traffic_Management_System_all) #2019,2020,2022,2024
head(Traffic_Volume) #2020~2024
head(Traffic_Volume_ver2) #2005~2024
head(Annual_Average_Daily_Traffic) #2019~2024
head(Drowsy_Driving) #2020~2024

#─────────────────────────연도를 잘 맞춰서 분석해야함───────────────────────────
#[고속도로 구조물]
years_to_keep <- c(2019, 2020, 2021, 2024)

Traffic_Accident_filtered <- Traffic_Accident_cnt %>%
  mutate(년도 = as.numeric(년도)) %>%  # 문자형 → 숫자형 변환
  filter(년도 %in% years_to_keep)

highway_structures_filtered <- highway_structures_all %>%
  filter(년도 %in% years_to_keep)

structures_df <- Traffic_Accident_filtered %>%
  left_join(highway_structures_filtered, by = c("년도", "노선"))

structures_df <- na.omit(structures_df)
head(structures_df)

# Poisson 회귀
model_pois <- glm(사고횟수 ~ 교량수.개소. + 터널수.개소. + 암거수.개소.,
                  family = "poisson", data = structures_df)
summary(model_pois)

#───────────────────────────────────────────────────────────────────────────────
#[교통관리시스템]
HTMS_filtered <- Highway_Traffic_Management_System_all %>%
  filter(년도 %in% c(2019, 2020, 2022, 2024))

# Traffic_Accident_cnt$년도를 integer로 변환
Traffic_Accident_cnt <- Traffic_Accident_cnt %>%
  mutate(년도 = as.integer(년도))

# HTMS_filtered$년도도 integer인지 확인
HTMS_filtered <- HTMS_filtered %>%
  mutate(년도 = as.integer(년도))

# 이제 join
Management_df <- Traffic_Accident_cnt %>%
  filter(년도 %in% c(2019, 2020, 2022, 2024)) %>%
  left_join(HTMS_filtered, by = c("년도", "노선"))

Management_df <- na.omit(Management_df)
head(Management_df)

model_pois <- glm(사고횟수 ~ 차량검지기.개수 + 도로전광표지.개수,
                  family = "poisson",
                  data = Management_df)
summary(model_pois)

#───────────────────────────────────────────────────────────────────────────────
#[교통량]
Traffic_Volume_long <- Traffic_Volume %>%
  pivot_longer(cols = starts_with("202"),   # 2020~2024 열
               names_to = "년도",
               values_to = "교통량") %>%
  mutate(년도 = as.integer(년도)) %>%       # 연도 정수형으로 변환
  group_by(구분, 년도) %>%
  summarise(교통량 = sum(교통량), .groups = "drop")  # 차종 합계
head(Traffic_Volume_long)

Traffic_Volume_long <- Traffic_Volume_long %>%
  mutate(노선 = gsub(" ", "", 구분)) %>%
  select(-구분)
head(Traffic_Volume_long)

Traffic_Accident_cnt <- Traffic_Accident_cnt %>%
  mutate(노선 = gsub(" ", "", 노선))  # 공백 제거
Traffic_Accident_cnt$년도 <- as.integer(Traffic_Accident_cnt$년도)

Traffic_Volume_df <- Traffic_Accident_cnt %>%
  left_join(Traffic_Volume_long, by = c("년도", "노선"))

Traffic_Volume_df <- na.omit(Traffic_Volume_df)
head(Traffic_Volume_df)

model_pois <- glm(사고횟수 ~ 교통량,
                  family = "poisson",
                  data = Traffic_Volume_df)

# 결과 확인
summary(model_pois)

#───────────────────────────────────────────────────────────────────────────────
#[연평균 일교통량]
Daily_Traffic_Volume_long <- Annual_Average_Daily_Traffic %>%
  pivot_longer(cols = starts_with("20"),     # 2019~2024 열
               names_to = "년도",
               values_to = "교통량") %>%
  mutate(년도 = as.integer(년도)) %>%
  group_by(노선, 년도) %>%
  summarise(교통량 = sum(교통량), .groups = "drop")

Traffic_Accident_cnt <- Traffic_Accident_cnt %>%
  mutate(년도 = as.integer(년도),
         노선 = gsub(" ", "", 노선))  # 공백 제거

Daily_Traffic_Volume_long <- Daily_Traffic_Volume_long %>%
  mutate(노선 = gsub(" ", "", 노선))

Daily_Traffic_df <- Traffic_Accident_cnt %>%
  left_join(Traffic_Volume_long, by = c("노선", "년도"))

Daily_Traffic_df <- na.omit(Daily_Traffic_df)
head(Daily_Traffic_df)

model_pois <- glm(사고횟수 ~ 교통량,
                  family = "poisson",
                  data = Daily_Traffic_df)

# 결과 확인
summary(model_pois)
#───────────────────────────────────────────────────────────────────────────────
#[졸음운전 사고수]
Drowsy_Driving_cnt <- Drowsy_Driving %>%
  group_by(년도, 노선) %>%
  summarise(졸음운전_사고수 = n(), .groups = "drop")  # n()는 행 개수 세기

Drowsy_Driving_cnt <- Drowsy_Driving_cnt %>%
  mutate(년도 = as.integer(년도))

head(Drowsy_Driving_cnt)

Drowsy_Driving_df <- Traffic_Accident_cnt %>%
  left_join(Drowsy_Driving_cnt, by = c("년도", "노선"))

Drowsy_Driving_df <- na.omit(Drowsy_Driving_df)
head(Drowsy_Driving_df)

model <- glm(사고횟수 ~ 졸음운전_사고수,
             family = "poisson",
             data = Drowsy_Driving_df)

summary(model)
