# 필요한 패키지 로드
library(dplyr)
library(rpart)
library(rpart.plot)

# 1️⃣ 데이터 준비
# final_df는 이미 숫자형 변수들로 되어 있다고 가정
# 사고횟수는 종속변수, 나머지는 독립변수
analysis_data <- final_df %>%
  ungroup() %>%                   # 그룹 해제
  select(-년도, -노선) %>%        # 그룹 컬럼 제거
  filter(if_all(everything(), ~ !is.na(.)))  # 결측값 제거

# 2️⃣ 트리 기반 회귀 분석
fit_tree <- rpart(
  사고횟수 ~ .,                  # 나머지 모든 변수를 독립변수로
  data = analysis_data,
  method = "anova"               # 연속형 종속변수용
)

# 3️⃣ 트리 시각화
rpart.plot(
  fit_tree,
  type = 3,                      # 노드 안에 번호 + 정보 표시
  extra = 101,                   # 각 노드에 예측값 표시
  fallen.leaves = TRUE,
  main = "사고횟수 예측 회귀 트리"
)

# 4️⃣ 트리 결과 확인
printcp(fit_tree)                 # 트리 복잡도 표
summary(fit_tree)                 # 상세 정보
