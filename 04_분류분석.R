# 필요한 패키지
library(dplyr) ; library(rpart) ; library(rpart.plot)

# 예: 중앙값 기준 High/Low 라벨 생성
Cluster_Analysis <- Cluster_Analysis %>%
  mutate(accident_level = ifelse(사고건수 > median(사고건수), "High", "Low"))

# 분류 트리 모델
tree_model <- rpart(accident_level ~ 사망자수 + 부상자수 + total_volume + 과속 + 졸음 + 주시태만,
                    data = Cluster_Analysis,
                    method = "class",
                    control = rpart.control(cp = 0.01))

# 트리 시각화
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE)

