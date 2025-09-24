library(factoextra)

#[변수 불러오기]
load("./r_data/Traffic_Accident.rdata")
load("./r_data/traffic_volume_2018.rdata")
load("./r_data/cause_of_traffic_accident_2022.rdata")
load("./r_data/drowsy_driving_2022.rdata")

#*[메인변수]
head(Traffic_Accident) #교통사고_2018

#*[사이드변수]
head(traffic_volume_2018) #교통량_2018
head(cause_of_traffic_accident_2022) #교통사고 원인_2022
head(drowsy_driving_2022) #졸음운전_2022

#───────────────────────────────────────────────────────────────────────────────
#[데이터 결합]
df <- Traffic_Accident %>%
  full_join(traffic_volume_2018, by = "노선명") %>%
  full_join(cause_of_traffic_accident_2022, by = "노선명") %>%
  full_join(drowsy_driving_2022, by = "노선명")
head(df)

df_clean <- na.omit(df)
head(df_clean)
str(df_clean)

#───────────────────────────────────────────────────────────────────────────────
#[군집분석]
Cluster_Analysis <- df_clean %>%
  select(사고건수, 사망자수, 부상자수, total_volume, 과속, 졸음, 주시태만)
head(Cluster_Analysis)

# 1. 사용할 데이터 선택
cluster_data <- Cluster_Analysis  # 이미 변수: 사고건수, 사망자수, 부상자수, total_volume, 과속, 졸음, 주시태만

# 2. 데이터 스케일링 (표준화)
cluster_data_scaled <- scale(cluster_data)

# 3. 최적 클러스터 수 찾기 (Elbow Method)
wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(cluster_data_scaled, centers = k, nstart = 25)$withinss)
}

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of Clusters K",
     ylab = "Total Within-Clusters Sum of Squares",
     main = "Elbow Method for Optimal K")

# 4. K-means 수행 (예: 3클러스터로 설정)
set.seed(123)  # 재현성 확보
k_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)

# 5. 클러스터 결과 확인
table(k_result$cluster)

# 6. 클러스터 시각화
fviz_cluster(k_result, data = cluster_data_scaled,
             geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

# 7. 클러스터별 평균값 확인
cluster_summary <- aggregate(cluster_data, 
                             by = list(cluster = k_result$cluster), 
                             FUN = mean)
print(cluster_summary)

