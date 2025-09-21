# 연도 타입 통일 및 노선 공백 제거
Traffic_Accident_cnt <- Traffic_Accident_cnt %>%
  mutate(년도 = as.integer(년도))

highway_structures_all <- highway_structures_all %>%
  mutate(년도 = as.integer(년도))

Highway_Traffic_Management_System_all <- Highway_Traffic_Management_System_all %>%
  mutate(년도 = as.integer(년도),
         노선 = gsub(" ", "", 노선))

Traffic_Volume_long <- Traffic_Volume %>%
  pivot_longer(cols = starts_with("2"), names_to = "년도", values_to = "교통량") %>%
  mutate(년도 = as.integer(년도),
         노선 = gsub(" ", "", 구분)) %>%
  group_by(년도, 노선) %>%
  summarise(교통량 = sum(교통량), .groups = "drop")

AADT_long <- Annual_Average_Daily_Traffic %>%
  pivot_longer(cols = starts_with("2"), names_to = "년도", values_to = "연평균일교통량") %>%
  mutate(년도 = as.integer(년도),
         노선 = gsub(" ", "", 노선)) %>%
  group_by(년도, 노선) %>%
  summarise(연평균일교통량 = sum(연평균일교통량), .groups = "drop")

Drowsy_Driving_cnt <- Drowsy_Driving %>%
  mutate(노선 = gsub(" ", "", 노선)) %>%
  group_by(년도, 노선) %>%
  summarise(졸음운전_사고수 = n(), .groups = "drop")

# 2020, 2024년도만 필터
Traffic_Accident_filtered <- Traffic_Accident_cnt %>% filter(년도 %in% c(2020, 2024))
highway_structures_filtered <- highway_structures_all %>% filter(년도 %in% c(2020, 2024))
HTMS_filtered <- Highway_Traffic_Management_System_all %>% filter(년도 %in% c(2020, 2024))
Traffic_Volume_filtered <- Traffic_Volume_long %>% filter(년도 %in% c(2020, 2024))
AADT_filtered <- AADT_long %>% filter(년도 %in% c(2020, 2024))
Drowsy_Driving_filtered <- Drowsy_Driving_cnt %>% filter(년도 %in% c(2020, 2024))

# 예: 모든 데이터프레임에서 년도를 integer로 변환
Traffic_Accident_filtered <- Traffic_Accident_filtered %>% mutate(년도 = as.integer(년도))
highway_structures_filtered <- highway_structures_filtered %>% mutate(년도 = as.integer(년도))
HTMS_filtered <- HTMS_filtered %>% mutate(년도 = as.integer(년도))
Traffic_Volume_filtered <- Traffic_Volume_filtered %>% mutate(년도 = as.integer(년도))
AADT_filtered <- AADT_filtered %>% mutate(년도 = as.integer(년도))
Drowsy_Driving_filtered <- Drowsy_Driving_filtered %>% mutate(년도 = as.integer(년도))

# full_join으로 합치기 (NA 발생 가능, 나중에 0 처리 가능)
final_df <- Traffic_Accident_filtered %>%
  full_join(highway_structures_filtered, by = c("년도", "노선")) %>%
  full_join(HTMS_filtered, by = c("년도", "노선")) %>%
  full_join(Traffic_Volume_filtered, by = c("년도", "노선")) %>%
  full_join(AADT_filtered, by = c("년도", "노선")) %>%
  full_join(Drowsy_Driving_filtered, by = c("년도", "노선"))

# NA를 0으로 채우기 (선택 사항)
final_df[is.na(final_df)] <- 0

# 결과 확인
head(final_df)

#───────────────────────────────────────────────────────────────────────────────
#[군집분석]
cluster_data <- final_df %>%
  ungroup() %>%                       # 그룹화 해제
  select(-년도, -노선)

cluster_data_scaled <- scale(cluster_data)

wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(cluster_data_scaled, centers = k, nstart = 25)$withinss)
}

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of Clusters K",
     ylab = "Total Within-Clusters Sum of Squares",
     main = "Elbow Method for Optimal K")

set.seed(123)  # 재현성을 위해
k_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)

# 클러스터 결과 확인
table(k_result$cluster)

fviz_cluster(k_result, data = cluster_data_scaled,
             geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

cluster_summary <- aggregate(cluster_data, 
                             by = list(cluster = k_result$cluster), 
                             FUN = mean)
print(cluster_summary)


