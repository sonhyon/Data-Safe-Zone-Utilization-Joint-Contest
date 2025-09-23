load("./r_data/Traffic_Accident_cnt.rdata")
load("./r_data/highway_structures_all.rdata")
load("./r_data/Highway_Traffic_Management_System_all.rdata")
load("./r_data/Traffic_Volume.rdata")
load("./r_data/Traffic_Volume_ver2.rdata")
load("./r_data/Annual_Average_Daily_Traffic.rdata")
load("./r_data/Drowsy_Driving.rdata")

#*[종속변수]
head(Traffic_Accident_cnt) #교통사고_2018~2024

#*[독립변수]
head(highway_structures_all) #2019~2021,2024
head(Highway_Traffic_Management_System_all) #2019,2020,2022,2024
head(Traffic_Volume) #2020~2024
head(Traffic_Volume_ver2) #2005~2024
head(Annual_Average_Daily_Traffic) #2019~2024
head(Drowsy_Driving) #2020~2024
#───────────────────────────────────────────────────────────────────────────────
#분산 확인_분산이 거의 없는 변수는 군집에 거의 영향 안 줌 (제외해야 함)

#-- 1년 단위의 데이터이면 분산 신경 안써도 됨
#-- 년단위 시계열 데이터면 선별해야됨 (ex. 터널수가 시간에 흐름에 따라 바뀌지 않으니까)



#───────────────────────────────────────────────────────────────────────────────
#상관확인_상관이 매우 높은 변수끼리는 중복 정보 -> 하나만 선택하거나 PCA 활용

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


# 필요한 패키지 불러오기
install.packages("Hmisc")
install.packages("corrplot")
library(Hmisc)
library(corrplot)
library(dplyr)

# 1. 숫자형 변수만 선택 (년도, 노선 제외)
numeric_df <- structures_df %>%
  dplyr::ungroup() %>%       # 그룹 해제
  dplyr::select(-년도, -노선)

# 2. 상관계수 계산
cor_matrix <- cor(numeric_df, use = "complete.obs", method = "pearson")
print(cor_matrix)

# 3. p-value 포함한 상관분석
res <- rcorr(as.matrix(numeric_df))
res$r   # 상관계수
res$P   # p-value

# 4. 시각화
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45)


#───────────────────────────────────────────────────────────────────────────────
#도메인 지식 적용_해당 변수들이 분석 목표와 관련 있는지 본인 판단



#───────────────────────────────────────────────────────────────────────────────
#시각화_변수 간 산점도, boxplot 등을 통해 분포 확인


