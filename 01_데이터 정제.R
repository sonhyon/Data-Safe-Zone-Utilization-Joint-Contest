library(dplyr) ; library(stringr) ; library(tidyr) ; library(factoextra)

getwd()
#───────────────────────────────────────────────────────────────────────────────
##*[교통사고_2018~2024]
Traffic_Accident <- read.csv("./데이터/종속변수_한국도로공사_고속도로 교통사고 상세현황_(2018~2024).csv", fileEncoding = "CP949")

names(Traffic_Accident) <- gsub("사고년도", "년도", names(Traffic_Accident))
names(Traffic_Accident) <- gsub("노선명", "노선", names(Traffic_Accident))
Traffic_Accident <- Traffic_Accident %>%
  mutate(년도 = str_replace(년도, "년", ""))

head(Traffic_Accident)

Traffic_Accident_cnt <- Traffic_Accident %>%
  group_by(년도, 노선) %>%
  summarise(사고횟수 = n())
head(Traffic_Accident_cnt)

#───────────────────────────────────────────────────────────────────────────────
#**[고속도로 구조물 현황_2019~2021,2024]
highway_structures_2019 <- read.csv("./데이터/독립변수_고속도로 구조물 현황(2019년).csv", fileEncoding = "CP949")
highway_structures_2020 <- read.csv("./데이터/독립변수_고속도로 구조물 현황(2020년).csv", fileEncoding = "CP949")
highway_structures_2021 <- read.csv("./데이터/독립변수_고속도로 구조물 현황(2021년).csv", fileEncoding = "CP949")
highway_structures_2024 <- read.csv("./데이터/독립변수_고속도로 구조물 현황(2024년).csv", fileEncoding = "CP949")

highway_structures_2019 <- mutate(highway_structures_2019, 사고년도 = 2019)
highway_structures_2020 <- mutate(highway_structures_2020, 사고년도 = 2020)
highway_structures_2021 <- mutate(highway_structures_2021, 사고년도 = 2021)
highway_structures_2024 <- mutate(highway_structures_2024, 사고년도 = 2024)

highway_structures_2019 <- highway_structures_2019 %>%
  select(-c(교량.개교._장대교.100m이상., 교량.개교._소교량.100m미만.))

names(highway_structures_2019) <- c(
  "노선",           # 기존: "노선"
  "교량수.개소.",    # 기존: "교량.개교._계"
  "터널수.개소.",    # 기존: "터널.개소."
  "암거수.개소.",    # 기존: "암거.개소."
  "사고년도"            # 기존: "year"
)

head(highway_structures_2019)
head(highway_structures_2020)
head(highway_structures_2021)
head(highway_structures_2024)

highway_structures_all <- bind_rows(
  highway_structures_2019,
  highway_structures_2020,
  highway_structures_2021,
  highway_structures_2024
)

names(highway_structures_all) <- gsub("사고년도", "년도", names(highway_structures_all))
head(highway_structures_all)

#───────────────────────────────────────────────────────────────────────────────
##*[교통관리시스템_2019,2020,2022,2024]
Highway_Traffic_Management_System_2019 <- read.csv("./데이터/독립변수_고속도로 노선별 교통관리시스템 현황(2019년).csv", fileEncoding = "CP949")
Highway_Traffic_Management_System_2020 <- read.csv("./데이터/독립변수_고속도로 노선별 교통관리시스템 현황(2020년).csv", fileEncoding = "CP949")
Highway_Traffic_Management_System_2022 <- read.csv("./데이터/독립변수_고속도로 노선별 교통관리시스템 현황(2022년).csv", fileEncoding = "CP949")
Highway_Traffic_Management_System_2024 <- read.csv("./데이터/독립변수_고속도로 노선별 교통관리시스템 현황(2024년).csv", fileEncoding = "CP949")

Highway_Traffic_Management_System_2019 <- Highway_Traffic_Management_System_2019 %>%
  select(-연.장) %>%       # 기존 열 제거
  rename(노선 = `노.선`, 구간 = `구.간`)

head(Highway_Traffic_Management_System_2019)
head(Highway_Traffic_Management_System_2020)
head(Highway_Traffic_Management_System_2022)
head(Highway_Traffic_Management_System_2024)

Highway_Traffic_Management_System_all <- bind_rows(
  Highway_Traffic_Management_System_2019,
  Highway_Traffic_Management_System_2020,
  Highway_Traffic_Management_System_2022,
  Highway_Traffic_Management_System_2024
)

names(Highway_Traffic_Management_System_all) <- gsub("기준년도", "년도", names(Highway_Traffic_Management_System_all))
Highway_Traffic_Management_System_all <- Highway_Traffic_Management_System_all %>%
  select(-구간) %>%
  mutate(노선 = str_replace_all(노선, " ", ""))
  
head(Highway_Traffic_Management_System_all)

#───────────────────────────────────────────────────────────────────────────────
##*[노선별 교통량_2020~2024]
Traffic_Volume <- read.csv("./데이터/독립변수_한국도로공사_노선별 교통량_(2020~2024).csv", fileEncoding = "CP949")

names(Traffic_Volume) <- gsub("X|년", "", names(Traffic_Volume))
head(Traffic_Volume)

#───────────────────────────────────────────────────────────────────────────────
##*[노선별 교통량_2005~2024]
Traffic_Volume_ver2 <- read.csv("./데이터/독립변수_한국도로공사_연도별노선별분석자료_(2005~2024).csv", fileEncoding = "CP949")

names(Traffic_Volume_ver2) <- gsub("X|년", "", names(Traffic_Volume_ver2))
head(Traffic_Volume_ver2)

#───────────────────────────────────────────────────────────────────────────────
##*[연평균 일교통량_2019~2024]
Annual_Average_Daily_Traffic <- read.csv("./데이터/독립변수_한국도로공사_연평균일교통량_(2019~2024).csv", fileEncoding = "CP949")

names(Annual_Average_Daily_Traffic) <- gsub("X", "", names(Annual_Average_Daily_Traffic))
head(Annual_Average_Daily_Traffic)

#───────────────────────────────────────────────────────────────────────────────
##*[졸음운전_2020~2024]
Drowsy_Driving <- read.csv("./데이터/독립변수_한국도로공사_졸음운전 사고통계_(2022~2024).csv", fileEncoding = "CP949")

Drowsy_Driving <- Drowsy_Driving %>%
  mutate(사고연도 = str_replace(사고연도, "년", ""))
names(Drowsy_Driving) <- gsub("사고연도", "년도", names(Drowsy_Driving))
names(Drowsy_Driving) <- gsub("노선명", "노선", names(Drowsy_Driving))
head(Drowsy_Driving)
