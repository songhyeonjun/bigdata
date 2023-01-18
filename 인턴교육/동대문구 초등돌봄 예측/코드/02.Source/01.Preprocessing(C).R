# Packages
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("imputeTS")
# install.packages("stringr")
# install.packages("readr")

options(warn=-1)
library(dplyr)
library(data.table)
library(imputeTS)
library(stringr)
library(readr)

# Step 1 : 아파트기본 정보 및 면적 정보 전처리 ---
# Step 1-1 : 아파트 기본 정보 데이터 전처리 ---
setwd("C:/Users/user/Desktop/초등돌봄수요예측/03.groupC/00.Data/01.아파트_기본_정보/")
apt_df_y1<- NULL
for(i in 1:length(dir())){
  data <- read.csv(dir()[i], stringsAsFactors = FALSE, fileEncoding = "euc-kr")
  names(data) <- c("ID", "건물명", "주소", "시도", "시군구", "준공년도", "준공년도_기준_행정동명", "시도1", "시군구1",	"준공년도_1년_기준_행정동명", "시도2",	"시군구2","준공년도_2년_기준_행정동명",
                   "시도3",	"시군구3","준공년도_3년_기준_행정동명","초등학교", "초등학교와의거리", "건설사", "세대당주차대수",
                   "용적률", "건폐율", "총세대수", "동수", "최고층", "최저층", "종합병원까지의거리", "대형마트까지의거리", "도서관까지의거리", "중학교까지의거리",
                   "난방방식", "난방종류", "현관구조", "마트", "편의점", "어린이집", "헬스장", "실내골프장", "배드민턴장", "무인택배함", "독서실", "카페", "농구장",
                   "유치원")
  apt_df_y1 <- rbind(apt_df_y1, data)
}
rm(data); rm(i);

names(apt_df_y1) <- c("ID", "건물명", "주소", "시도", "시군구", "준공년도", "준공년도_기준_행정동명", "시도1", "시군구1",	"준공년도_1년_기준_행정동명", "시도2",	"시군구2","준공년도_2년_기준_행정동명",
                      "시도3",	"시군구3","준공년도_3년_기준_행정동명","초등학교", "초등학교와의거리", "건설사", "세대당주차대수",
                      "용적률", "건폐율", "총세대수", "동수", "최고층", "최저층", "종합병원까지의거리", "대형마트까지의거리", "도서관까지의거리", "중학교까지의거리",
                      "난방방식", "난방종류", "현관구조", "마트", "편의점", "어린이집", "헬스장", "실내골프장", "배드민턴장", "무인택배함", "독서실", "카페", "농구장",
                      "유치원")
apt_df_y1$건물명 <- str_trim(apt_df_y1$건물명, side= c("both"))
apt_df_y1$주소 <- str_trim(apt_df_y1$주소, side= c("both"))
apt_df_y1$시도 <- str_trim(apt_df_y1$시도, side= c("both"))
apt_df_y1$시군구 <- str_trim(apt_df_y1$시군구, side= c("both"))
apt_df_y1$준공년도_기준_행정동명 <- str_trim(apt_df_y1$준공년도_기준_행정동명, side= c("both"))
apt_df_y1$준공년도_1년_기준_행정동명 <- str_trim(apt_df_y1$준공년도_1년_기준_행정동명, side= c("both"))
apt_df_y1$준공년도_2년_기준_행정동명 <- str_trim(apt_df_y1$준공년도_2년_기준_행정동명, side= c("both"))
apt_df_y1$준공년도_3년_기준_행정동명 <- str_trim(apt_df_y1$준공년도_3년_기준_행정동명, side= c("both"))
apt_df_y1$초등학교 <- str_trim(apt_df_y1$초등학교, side= c("both"))
apt_df_y1$건설사 <- str_trim(apt_df_y1$건설사, side= c("both"))
apt_df_y1$난방방식 <- str_trim(apt_df_y1$난방방식, side= c("both"))
apt_df_y1$난방종류 <- str_trim(apt_df_y1$난방종류, side= c("both"))
apt_df_y1$현관구조 <- str_trim(apt_df_y1$현관구조, side= c("both"))
apt_df_y1$초등학교와의거리 <- as.integer(apt_df_y1$초등학교와의거리)
apt_df_y1[is.na(apt_df_y1$시군구), "시군구"] <- ""
apt_df_y1[is.na(apt_df_y1$시군구1), "시군구1"] <- ""
apt_df_y1[is.na(apt_df_y1$시군구2), "시군구2"] <- ""
apt_df_y1[is.na(apt_df_y1$시군구2), "시군구3"] <- ""

# 난방방식 전처리
apt_df_y1$난방방식 <- ifelse(apt_df_y1$난방방식 == "개별", "개별난방", apt_df_y1$난방방식)
apt_df_y1$난방방식 <- ifelse(apt_df_y1$난방방식 == "개발난방", "개별난방", apt_df_y1$난방방식)

# 난방종류 전처리
apt_df_y1$난방종류 <- ifelse(apt_df_y1$난방종류 == "도시", "도시가스", apt_df_y1$난방종류)
apt_df_y1$난방종류 <- ifelse(apt_df_y1$난방종류 == "LPG가스", "LPG", apt_df_y1$난방종류)
apt_df_y1$난방종류 <- ifelse(apt_df_y1$난방종류 == "lpg", "LPG", apt_df_y1$난방종류)
apt_df_y1$난방종류 <- ifelse(apt_df_y1$난방종류 == "LPG", "도시가스", apt_df_y1$난방종류)
apt_df_y1$난방종류 <- ifelse(apt_df_y1$난방종류 == "lng", "도시가스", apt_df_y1$난방종류)
apt_df_y1$난방종류 <- ifelse(apt_df_y1$난방종류 == "LNG", "도시가스", apt_df_y1$난방종류)
apt_df_y1$난방종류 <- ifelse(apt_df_y1$난방종류 == "지역난방", "열병합", apt_df_y1$난방종류)
apt_df_y1$난방종류 <- ifelse(apt_df_y1$난방종류 == "태양열", "열병합", apt_df_y1$난방종류)
# 현관구조 전처리
apt_df_y1$현관구조 <- ifelse(apt_df_y1$현관구조 == "계단", "계단식", apt_df_y1$현관구조)
apt_df_y1$현관구조 <- ifelse(apt_df_y1$현관구조 == "혼합식", "복합식", apt_df_y1$현관구조)

cat(' ','\n')
print('######## Step 1 : 아파트 기본 정보 데이터 전처리 완료 ########')
cat(' ','\n')

# Step 1-2 : 아파트 면적 정보 데이터 로드 ---
setwd("C:/Users/user/Desktop/초등돌봄수요예측/03.groupC/00.Data/02.아파트_면적_정보/")
apt_df_y2<- NULL
for(i in 1:length(dir())){
  data <- read.csv(dir()[i], stringsAsFactors = FALSE, fileEncoding = "euc-kr")
  names(data) <- c("ID",	"건물명",	"전용면적",	"방수",	"욕실수",	"세대수")
  apt_df_y2 <- rbind(apt_df_y2, data)
}
rm(data); rm(i);
apt_df_y2$건물명 <- str_trim(apt_df_y2$건물명, side = c('both'))

# 평균면적 계산
apt_df_y2$면적 <- apt_df_y2$전용면적 * apt_df_y2$세대수
# 총면적
apt_df_square_gr <- as.data.frame(apt_df_y2 %>%
                                    group_by(ID = ID) %>%
                                    summarise(총면적 = sum(면적)))
# 아파트 정보 결합
apt_info <- merge(apt_df_y1, apt_df_square_gr, by = c('ID'), all.x = TRUE)
# 평균면적
apt_info$평균면적 <- round(apt_info$총면적 / apt_info$총세대수, 2)
# 면적별 세대수 계산
apt_df_y2[apt_df_y2$전용면적 <= 67.44, '면적구간'] <- paste("면적", 67.44, "세대수", sep = "")
apt_df_y2[apt_df_y2$전용면적 > 67.44 & apt_df_y2$전용면적 <= 80.07, '면적구간'] <- paste("면적", 67.44, '_', 80.07, "세대수", sep = "")
apt_df_y2[apt_df_y2$전용면적 > 80.07 & apt_df_y2$전용면적 <= 86.52, '면적구간'] <- paste("면적", 80.07, '_', 86.52, "세대수", sep = "")
apt_df_y2[apt_df_y2$전용면적 > 86.52, '면적구간'] <- paste("면적", 86.52, "세대수", sep = "")

apt_square_fami_gr <- apt_df_y2 %>%
  group_by(ID=ID, 면적구간) %>%
  summarise(면적별세대수 = sum(세대수))

apt_square_fami <- dcast(apt_square_fami_gr, ID ~ 면적구간, value.var = '면적별세대수', drop = FALSE)
apt_square_fami <- na_replace(apt_square_fami, 0)

apt_info <- merge(apt_info, apt_square_fami, by = c('ID'), all.x = TRUE)

# 방수 계산
apt_df_y2$전체방수 <- apt_df_y2$방수 * apt_df_y2$세대수
apt_df_square_gr <- as.data.frame(apt_df_y2 %>%
                                    group_by(ID = ID) %>%
                                    summarise(총방수 = sum(전체방수)))
apt_info <- merge(apt_info, apt_df_square_gr, by = c("ID"), all.x = TRUE)
apt_info$평균방수 <- round(apt_info$총방수 / apt_info$총세대수, 2)

# 욕실 수 계산
apt_df_y2$전체욕실수 <- apt_df_y2$욕실수 * apt_df_y2$세대수
apt_df_square_gr <- as.data.frame(apt_df_y2 %>%
                                    group_by(ID = ID) %>%
                                    summarise(총욕실수 = sum(전체욕실수)))
apt_info <- merge(apt_info, apt_df_square_gr, by = c("ID"), all.x = TRUE)
apt_info$평균욕실수 <- round(apt_info$총욕실수 / apt_info$총세대수, 2)

apt_info <- data.frame(apt_info)
# 아파트 준공년도, -1, -2, -3년 도출
apt_info$준공년도 <- as.integer(apt_info$준공년도)
apt_info$준공년도_m1 <- apt_info$준공년도 - 1
apt_info$준공년도_m2 <- apt_info$준공년도 - 2
apt_info$준공년도_m3 <- apt_info$준공년도 - 3
cat(' ','\n')
print('######## Step 2 : 아파트 면적 정보 데이터 로드 완료 ########')
cat(' ','\n')

# Step 2 : 인구이동통계 코드집 시군구 코드 데이터 로딩 ---
setwd("C:/Users/user/Desktop/초등돌봄수요예측/03.groupC/00.Data/03.인구이동통계_코드집/")
raw_city_code <- read.csv("C:/Users/user/Desktop/초등돌봄수요예측/03.groupC/00.Data/03.인구이동통계_코드집/2019_인구이동통계_코드집_공공용.csv", stringsAsFactor=FALSE, fileEncoding = "cp949")
raw_city_code <- as.data.frame(raw_city_code)
# 시도 시군구 읍면동 코드 추출
raw_city_code$sido_cd <- substring(raw_city_code$행자부코드, 1,2)
raw_city_code$sgg_cd <- substring(raw_city_code$행자부코드, 3, 5)
raw_city_code$emd_cd <- substring(raw_city_code$행자부코드, 6, 10)
# 시도 시군구 읍면동 코드 형태 변환
raw_city_code$sido_cd <- as.integer(raw_city_code$sido_cd)
raw_city_code$sgg_cd <- as.integer(raw_city_code$sgg_cd)
raw_city_code$emd_cd <- as.integer(raw_city_code$emd_cd)

cat(' ','\n')
print('######## Step 3 : 인구이동통계 코드집 시군구 코드 데이터 로딩 완료 ########')
cat(' ','\n')

load("C:/Users/user/Desktop/초등돌봄수요예측/03.groupC/01.Preprocessing/01.pre_data1/년도_동별_전출입건수.RData")
load("C:/Users/user/Desktop/초등돌봄수요예측/03.groupC/01.Preprocessing/01.pre_data1/동_나이별_전출입_인구차이.RData")
load("C:/Users/user/Desktop/초등돌봄수요예측/03.groupC/01.Preprocessing/02.pre_data2/동별_인구_추이_데이터.RData")

cat(' ','\n')
print('######## Step 4 : 분석 데이터 전체 로딩 완료 ########')
cat(' ','\n')

# Step 6 : 분석용 데이터 셋 생성 ---
# Step 6-1 : 전출입 건수 추이 데이터 셋 통합 ---
apt_info$준공년도_m1 <- as.integer(apt_info$준공년도_m1)
apt_info$준공년도_m2 <- as.integer(apt_info$준공년도_m2)
apt_info$준공년도_m3 <- as.integer(apt_info$준공년도_m3)

# 데이터 결합
apt_info_pop_inoutcnt <- merge(apt_info, anal_move_total, by.x = c("준공년도_m1", "시도1", "시군구1", "준공년도_1년_기준_행정동명"), by.y = c("년도", "시도", "시군구", "읍면동"), all.x = TRUE)
apt_info_pop_inoutcnt <- merge(apt_info_pop_inoutcnt, anal_move_total, by.x = c("준공년도_m2", "시도2", "시군구2", "준공년도_2년_기준_행정동명"), by.y = c("년도", "시도", "시군구", "읍면동"), all.x = TRUE, suffix = c("", "_1"))
apt_info_pop_inoutcnt <- merge(apt_info_pop_inoutcnt, anal_move_total, by.x = c("준공년도_m3", "시도3", "시군구3", "준공년도_3년_기준_행정동명"), by.y = c("년도", "시도", "시군구", "읍면동"), all.x = TRUE, suffix = c("", "_2"))
apt_info_pop_inoutcnt <- merge(apt_info_pop_inoutcnt, anal_move_in_out_total_diff, by.x = c("준공년도_m1", "시도1", "시군구1", "준공년도_1년_기준_행정동명"), by.y = c("전입년", "시도", "시군구", "읍면동"), all.x = TRUE)
apt_info_pop_inoutcnt <- merge(apt_info_pop_inoutcnt, anal_move_in_out_total_diff, by.x = c("준공년도_m2", "시도2", "시군구2", "준공년도_2년_기준_행정동명"), by.y = c("전입년", "시도", "시군구", "읍면동"), all.x = TRUE, suffix = c("", "_1"))
apt_info_pop_inoutcnt <- merge(apt_info_pop_inoutcnt, anal_move_in_out_total_diff, by.x = c("준공년도_m3", "시도3", "시군구3", "준공년도_3년_기준_행정동명"), by.y = c("전입년", "시도", "시군구", "읍면동"), all.x = TRUE, suffix = c("", "_2"))

# apt_info_pop_inoutcnt 내 전체 전입수/전출수  전체 전입수/전출수 차이 산출
apt_info_pop_inoutcnt$전체_전입수_차이_1 <- apt_info_pop_inoutcnt$전체_전입수 - apt_info_pop_inoutcnt$전체_전입수_1
apt_info_pop_inoutcnt$시내_전입수_차이_1 <- apt_info_pop_inoutcnt$시내_전입수 - apt_info_pop_inoutcnt$시내_전입수_1
apt_info_pop_inoutcnt$시외_전입수_차이_1 <- apt_info_pop_inoutcnt$시외_전입수 - apt_info_pop_inoutcnt$시외_전입수_1
apt_info_pop_inoutcnt$전체_전출수_차이_1 <- apt_info_pop_inoutcnt$전체_전출수 - apt_info_pop_inoutcnt$전체_전출수_1
apt_info_pop_inoutcnt$시내_전출수_차이_1 <- apt_info_pop_inoutcnt$시내_전출수 - apt_info_pop_inoutcnt$시내_전출수_1
apt_info_pop_inoutcnt$시외_전출수_차이_1 <- apt_info_pop_inoutcnt$시외_전출수 - apt_info_pop_inoutcnt$시외_전출수_1

# apt_info_pop_inoutcnt 내 전체 전입수/전출수1  전체 전입수/전출수2 차이 산출
apt_info_pop_inoutcnt$전체_전입수_차이_2 <- apt_info_pop_inoutcnt$전체_전입수_1 - apt_info_pop_inoutcnt$전체_전입수_2
apt_info_pop_inoutcnt$시내_전입수_차이_2 <- apt_info_pop_inoutcnt$시내_전입수_1 - apt_info_pop_inoutcnt$시내_전입수_2
apt_info_pop_inoutcnt$시외_전입수_차이_2 <- apt_info_pop_inoutcnt$시외_전입수_1 - apt_info_pop_inoutcnt$시외_전입수_2
apt_info_pop_inoutcnt$전체_전출수_차이_2 <- apt_info_pop_inoutcnt$전체_전출수_1 - apt_info_pop_inoutcnt$전체_전출수_2
apt_info_pop_inoutcnt$시내_전출수_차이_2 <- apt_info_pop_inoutcnt$시내_전출수_1 - apt_info_pop_inoutcnt$시내_전출수_2
apt_info_pop_inoutcnt$시외_전출수_차이_2 <- apt_info_pop_inoutcnt$시외_전출수_1 - apt_info_pop_inoutcnt$시외_전출수_2

# apt_info_pop_inoutcnt 내의 전입 건수 산출
apt_info_pop_inoutcnt$전입_6_차이_1 <- apt_info_pop_inoutcnt$전입_6 - apt_info_pop_inoutcnt$전입_6_1
apt_info_pop_inoutcnt$전입_시내_6_차이_1 <- apt_info_pop_inoutcnt$전입_시내_6 - apt_info_pop_inoutcnt$전입_시내_6_1
apt_info_pop_inoutcnt$전입_시외_6_차이_1 <- apt_info_pop_inoutcnt$전입_시외_6 - apt_info_pop_inoutcnt$전입_시외_6_1
apt_info_pop_inoutcnt$전입_7_차이_1 <- apt_info_pop_inoutcnt$전입_7 - apt_info_pop_inoutcnt$전입_7_1
apt_info_pop_inoutcnt$전입_시내_7_차이_1 <- apt_info_pop_inoutcnt$전입_시내_7 - apt_info_pop_inoutcnt$전입_시내_7_1
apt_info_pop_inoutcnt$전입_시외_7_차이_1 <- apt_info_pop_inoutcnt$전입_시외_7 - apt_info_pop_inoutcnt$전입_시외_7_1
apt_info_pop_inoutcnt$전입_8_차이_1 <- apt_info_pop_inoutcnt$전입_8 - apt_info_pop_inoutcnt$전입_8_1
apt_info_pop_inoutcnt$전입_시내_8_차이_1 <- apt_info_pop_inoutcnt$전입_시내_8 - apt_info_pop_inoutcnt$전입_시내_8_1
apt_info_pop_inoutcnt$전입_시외_8_차이_1 <- apt_info_pop_inoutcnt$전입_시외_8 - apt_info_pop_inoutcnt$전입_시외_8_1
apt_info_pop_inoutcnt$전입_9_차이_1 <- apt_info_pop_inoutcnt$전입_9 - apt_info_pop_inoutcnt$전입_9_1
apt_info_pop_inoutcnt$전입_시내_9_차이_1 <- apt_info_pop_inoutcnt$전입_시내_9 - apt_info_pop_inoutcnt$전입_시내_9_1
apt_info_pop_inoutcnt$전입_시외_9_차이_1 <- apt_info_pop_inoutcnt$전입_시외_9 - apt_info_pop_inoutcnt$전입_시외_9_1
apt_info_pop_inoutcnt$전입_10_차이_1 <- apt_info_pop_inoutcnt$전입_10 - apt_info_pop_inoutcnt$전입_10_1
apt_info_pop_inoutcnt$전입_시내_10_차이_1 <- apt_info_pop_inoutcnt$전입_시내_10 - apt_info_pop_inoutcnt$전입_시내_10_1
apt_info_pop_inoutcnt$전입_시외_10_차이_1 <- apt_info_pop_inoutcnt$전입_시외_10 - apt_info_pop_inoutcnt$전입_시외_10_1
apt_info_pop_inoutcnt$전입_11_차이_1 <- apt_info_pop_inoutcnt$전입_11 - apt_info_pop_inoutcnt$전입_11_1
apt_info_pop_inoutcnt$전입_시내_11_차이_1 <- apt_info_pop_inoutcnt$전입_시내_11 - apt_info_pop_inoutcnt$전입_시내_11_1
apt_info_pop_inoutcnt$전입_시외_11_차이_1 <- apt_info_pop_inoutcnt$전입_시외_11 - apt_info_pop_inoutcnt$전입_시외_11_1
apt_info_pop_inoutcnt$전입_전체_6_11_합_차이_1 <- apt_info_pop_inoutcnt$전입_전체_6_11_합 - apt_info_pop_inoutcnt$전입_전체_6_11_합_1
apt_info_pop_inoutcnt$전입_시내_6_11_합_차이_1 <- apt_info_pop_inoutcnt$전입_시내_6_11_합 - apt_info_pop_inoutcnt$전입_시내_6_11_합_1
apt_info_pop_inoutcnt$전입_시외_6_11_합_차이_1 <- apt_info_pop_inoutcnt$전입_시외_6_11_합 - apt_info_pop_inoutcnt$전입_시외_6_11_합_1

apt_info_pop_inoutcnt$전입_6_차이_2 <- apt_info_pop_inoutcnt$전입_6_1 - apt_info_pop_inoutcnt$전입_6_2
apt_info_pop_inoutcnt$전입_시내_6_차이_2 <- apt_info_pop_inoutcnt$전입_시내_6_1 - apt_info_pop_inoutcnt$전입_시내_6_2
apt_info_pop_inoutcnt$전입_시외_6_차이_2 <- apt_info_pop_inoutcnt$전입_시외_6_1 - apt_info_pop_inoutcnt$전입_시외_6_2
apt_info_pop_inoutcnt$전입_7_차이_2 <- apt_info_pop_inoutcnt$전입_7_1 - apt_info_pop_inoutcnt$전입_7_2
apt_info_pop_inoutcnt$전입_시내_7_차이_2 <- apt_info_pop_inoutcnt$전입_시내_7_1 - apt_info_pop_inoutcnt$전입_시내_7_2
apt_info_pop_inoutcnt$전입_시외_7_차이_2 <- apt_info_pop_inoutcnt$전입_시외_7_1 - apt_info_pop_inoutcnt$전입_시외_7_2
apt_info_pop_inoutcnt$전입_8_차이_2 <- apt_info_pop_inoutcnt$전입_8_1 - apt_info_pop_inoutcnt$전입_8_2
apt_info_pop_inoutcnt$전입_시내_8_차이_2 <- apt_info_pop_inoutcnt$전입_시내_8_1 - apt_info_pop_inoutcnt$전입_시내_8_2
apt_info_pop_inoutcnt$전입_시외_8_차이_2 <- apt_info_pop_inoutcnt$전입_시외_8_1 - apt_info_pop_inoutcnt$전입_시외_8_2
apt_info_pop_inoutcnt$전입_9_차이_2 <- apt_info_pop_inoutcnt$전입_9_1 - apt_info_pop_inoutcnt$전입_9_2
apt_info_pop_inoutcnt$전입_시내_9_차이_2 <- apt_info_pop_inoutcnt$전입_시내_9_1 - apt_info_pop_inoutcnt$전입_시내_9_2
apt_info_pop_inoutcnt$전입_시외_9_차이_2 <- apt_info_pop_inoutcnt$전입_시외_9_1 - apt_info_pop_inoutcnt$전입_시외_9_2
apt_info_pop_inoutcnt$전입_10_차이_2 <- apt_info_pop_inoutcnt$전입_10_1 - apt_info_pop_inoutcnt$전입_10_2
apt_info_pop_inoutcnt$전입_시내_10_차이_2 <- apt_info_pop_inoutcnt$전입_시내_10_1 - apt_info_pop_inoutcnt$전입_시내_10_2
apt_info_pop_inoutcnt$전입_시외_10_차이_2 <- apt_info_pop_inoutcnt$전입_시외_10_1 - apt_info_pop_inoutcnt$전입_시외_10_2
apt_info_pop_inoutcnt$전입_11_차이_2 <- apt_info_pop_inoutcnt$전입_11_1 - apt_info_pop_inoutcnt$전입_11_2
apt_info_pop_inoutcnt$전입_시내_11_차이_2 <- apt_info_pop_inoutcnt$전입_시내_11_1 - apt_info_pop_inoutcnt$전입_시내_11_2
apt_info_pop_inoutcnt$전입_시외_11_차이_2 <- apt_info_pop_inoutcnt$전입_시외_11_1 - apt_info_pop_inoutcnt$전입_시외_11_2
apt_info_pop_inoutcnt$전입_전체_6_11_합_차이_2 <- apt_info_pop_inoutcnt$전입_전체_6_11_합_1 - apt_info_pop_inoutcnt$전입_전체_6_11_합_2
apt_info_pop_inoutcnt$전입_시내_6_11_합_차이_2 <- apt_info_pop_inoutcnt$전입_시내_6_11_합_1 - apt_info_pop_inoutcnt$전입_시내_6_11_합_2
apt_info_pop_inoutcnt$전입_시외_6_11_합_차이_2 <- apt_info_pop_inoutcnt$전입_시외_6_11_합_1 - apt_info_pop_inoutcnt$전입_시외_6_11_합_2

# apt_info_pop_inoutcnt 내의 전출 건수 산출
apt_info_pop_inoutcnt$전출_6_차이_1 <- apt_info_pop_inoutcnt$전출_6 - apt_info_pop_inoutcnt$전출_6_1
apt_info_pop_inoutcnt$전출_시내_6_차이_1 <- apt_info_pop_inoutcnt$전출_시내_6 - apt_info_pop_inoutcnt$전출_시내_6_1
apt_info_pop_inoutcnt$전출_시외_6_차이_1 <- apt_info_pop_inoutcnt$전출_시외_6 - apt_info_pop_inoutcnt$전출_시외_6_1
apt_info_pop_inoutcnt$전출_7_차이_1 <- apt_info_pop_inoutcnt$전출_7 - apt_info_pop_inoutcnt$전출_7_1
apt_info_pop_inoutcnt$전출_시내_7_차이_1 <- apt_info_pop_inoutcnt$전출_시내_7 - apt_info_pop_inoutcnt$전출_시내_7_1
apt_info_pop_inoutcnt$전출_시외_7_차이_1 <- apt_info_pop_inoutcnt$전출_시외_7 - apt_info_pop_inoutcnt$전출_시외_7_1
apt_info_pop_inoutcnt$전출_8_차이_1 <- apt_info_pop_inoutcnt$전출_8 - apt_info_pop_inoutcnt$전출_8_1
apt_info_pop_inoutcnt$전출_시내_8_차이_1 <- apt_info_pop_inoutcnt$전출_시내_8 - apt_info_pop_inoutcnt$전출_시내_8_1
apt_info_pop_inoutcnt$전출_시외_8_차이_1 <- apt_info_pop_inoutcnt$전출_시외_8 - apt_info_pop_inoutcnt$전출_시외_8_1
apt_info_pop_inoutcnt$전출_9_차이_1 <- apt_info_pop_inoutcnt$전출_9 - apt_info_pop_inoutcnt$전출_9_1
apt_info_pop_inoutcnt$전출_시내_9_차이_1 <- apt_info_pop_inoutcnt$전출_시내_9 - apt_info_pop_inoutcnt$전출_시내_9_1
apt_info_pop_inoutcnt$전출_시외_9_차이_1 <- apt_info_pop_inoutcnt$전출_시외_9 - apt_info_pop_inoutcnt$전출_시외_9_1
apt_info_pop_inoutcnt$전출_10_차이_1 <- apt_info_pop_inoutcnt$전출_10 - apt_info_pop_inoutcnt$전출_10_1
apt_info_pop_inoutcnt$전출_시내_10_차이_1 <- apt_info_pop_inoutcnt$전출_시내_10 - apt_info_pop_inoutcnt$전출_시내_10_1
apt_info_pop_inoutcnt$전출_시외_10_차이_1 <- apt_info_pop_inoutcnt$전출_시외_10 - apt_info_pop_inoutcnt$전출_시외_10_1
apt_info_pop_inoutcnt$전출_11_차이_1 <- apt_info_pop_inoutcnt$전출_11 - apt_info_pop_inoutcnt$전출_11_1
apt_info_pop_inoutcnt$전출_시내_11_차이_1 <- apt_info_pop_inoutcnt$전출_시내_11 - apt_info_pop_inoutcnt$전출_시내_11_1
apt_info_pop_inoutcnt$전출_시외_11_차이_1 <- apt_info_pop_inoutcnt$전출_시외_11 - apt_info_pop_inoutcnt$전출_시외_11_1
apt_info_pop_inoutcnt$전출_전체_6_11_합_차이_1 <- apt_info_pop_inoutcnt$전출_전체_6_11_합 - apt_info_pop_inoutcnt$전출_전체_6_11_합_1
apt_info_pop_inoutcnt$전출_시내_6_11_합_차이_1 <- apt_info_pop_inoutcnt$전출_시내_6_11_합 - apt_info_pop_inoutcnt$전출_시내_6_11_합_1
apt_info_pop_inoutcnt$전출_시외_6_11_합_차이_1 <- apt_info_pop_inoutcnt$전출_시외_6_11_합 - apt_info_pop_inoutcnt$전출_시외_6_11_합_1

apt_info_pop_inoutcnt$전출_6_차이_2 <- apt_info_pop_inoutcnt$전출_6_1 - apt_info_pop_inoutcnt$전출_6_2
apt_info_pop_inoutcnt$전출_시내_6_차이_2 <- apt_info_pop_inoutcnt$전출_시내_6_1 - apt_info_pop_inoutcnt$전출_시내_6_2
apt_info_pop_inoutcnt$전출_시외_6_차이_2 <- apt_info_pop_inoutcnt$전출_시외_6_1 - apt_info_pop_inoutcnt$전출_시외_6_2
apt_info_pop_inoutcnt$전출_7_차이_2 <- apt_info_pop_inoutcnt$전출_7_1 - apt_info_pop_inoutcnt$전출_7_2
apt_info_pop_inoutcnt$전출_시내_7_차이_2 <- apt_info_pop_inoutcnt$전출_시내_7_1 - apt_info_pop_inoutcnt$전출_시내_7_2
apt_info_pop_inoutcnt$전출_시외_7_차이_2 <- apt_info_pop_inoutcnt$전출_시외_7_1 - apt_info_pop_inoutcnt$전출_시외_7_2
apt_info_pop_inoutcnt$전출_8_차이_2 <- apt_info_pop_inoutcnt$전출_8_1 - apt_info_pop_inoutcnt$전출_8_2
apt_info_pop_inoutcnt$전출_시내_8_차이_2 <- apt_info_pop_inoutcnt$전출_시내_8_1 - apt_info_pop_inoutcnt$전출_시내_8_2
apt_info_pop_inoutcnt$전출_시외_8_차이_2 <- apt_info_pop_inoutcnt$전출_시외_8_1 - apt_info_pop_inoutcnt$전출_시외_8_2
apt_info_pop_inoutcnt$전출_9_차이_2 <- apt_info_pop_inoutcnt$전출_9_1 - apt_info_pop_inoutcnt$전출_9_2
apt_info_pop_inoutcnt$전출_시내_9_차이_2 <- apt_info_pop_inoutcnt$전출_시내_9_1 - apt_info_pop_inoutcnt$전출_시내_9_2
apt_info_pop_inoutcnt$전출_시외_9_차이_2 <- apt_info_pop_inoutcnt$전출_시외_9_1 - apt_info_pop_inoutcnt$전출_시외_9_2
apt_info_pop_inoutcnt$전출_10_차이_2 <- apt_info_pop_inoutcnt$전출_10_1 - apt_info_pop_inoutcnt$전출_10_2
apt_info_pop_inoutcnt$전출_시내_10_차이_2 <- apt_info_pop_inoutcnt$전출_시내_10_1 - apt_info_pop_inoutcnt$전출_시내_10_2
apt_info_pop_inoutcnt$전출_시외_10_차이_2 <- apt_info_pop_inoutcnt$전출_시외_10_1 - apt_info_pop_inoutcnt$전출_시외_10_2
apt_info_pop_inoutcnt$전출_11_차이_2 <- apt_info_pop_inoutcnt$전출_11_1 - apt_info_pop_inoutcnt$전출_11_2
apt_info_pop_inoutcnt$전출_시내_11_차이_2 <- apt_info_pop_inoutcnt$전출_시내_11_1 - apt_info_pop_inoutcnt$전출_시내_11_2
apt_info_pop_inoutcnt$전출_시외_11_차이_2 <- apt_info_pop_inoutcnt$전출_시외_11_1 - apt_info_pop_inoutcnt$전출_시외_11_2
apt_info_pop_inoutcnt$전출_전체_6_11_합_차이_2 <- apt_info_pop_inoutcnt$전출_전체_6_11_합_1 - apt_info_pop_inoutcnt$전출_전체_6_11_합_2
apt_info_pop_inoutcnt$전출_시내_6_11_합_차이_2 <- apt_info_pop_inoutcnt$전출_시내_6_11_합_1 - apt_info_pop_inoutcnt$전출_시내_6_11_합_2
apt_info_pop_inoutcnt$전출_시외_6_11_합_차이_2 <- apt_info_pop_inoutcnt$전출_시외_6_11_합_1 - apt_info_pop_inoutcnt$전출_시외_6_11_합_2

#Step 6-2 : 동별 인구수 추이 데이터 셋 통합 ----
apt_info_pop_inoutcnt <- merge(apt_info_pop_inoutcnt, anal_dong_pop_diff, by.x = c("준공년도_m1", "시도1", "시군구1", "준공년도_1년_기준_행정동명"), by.y = c("년도", "시도", "시군구", "읍면동"), all.x = TRUE)
apt_info_pop_inoutcnt <- merge(apt_info_pop_inoutcnt, anal_dong_pop_diff, by.x = c("준공년도_m2", "시도2", "시군구2", "준공년도_2년_기준_행정동명"), by.y = c("년도", "시도", "시군구", "읍면동"), all.x = TRUE, suffix = c("", "_1"))
apt_info_pop_inoutcnt <- merge(apt_info_pop_inoutcnt, anal_dong_pop_diff, by.x = c("준공년도_m3", "시도3", "시군구3", "준공년도_3년_기준_행정동명"), by.y = c("년도", "시도", "시군구", "읍면동"), all.x = TRUE, suffix = c("", "_2"))

apt_info_pop_inoutcnt$동별_인구_차이_1 <- apt_info_pop_inoutcnt$동별_인구 - apt_info_pop_inoutcnt$동별_인구_1
apt_info_pop_inoutcnt$동별_인구_6_11_차이_1 <- apt_info_pop_inoutcnt$동별_인구_6_11 - apt_info_pop_inoutcnt$동별_인구_6_11_1

apt_info_pop_inoutcnt$동별_인구_차이_2 <- apt_info_pop_inoutcnt$동별_인구_1 - apt_info_pop_inoutcnt$동별_인구_2
apt_info_pop_inoutcnt$동별_인구_6_11_차이_2 <- apt_info_pop_inoutcnt$동별_인구_6_11_1 - apt_info_pop_inoutcnt$동별_인구_6_11_2

apt_info_pop_inoutcnt$동별_6_11_비율 <- round(apt_info_pop_inoutcnt$동별_인구_6_11/apt_info_pop_inoutcnt$동별_인구 * 100,2)
apt_info_pop_inoutcnt$동별_6_11_1_비율 <- round(apt_info_pop_inoutcnt$동별_인구_6_11_1/apt_info_pop_inoutcnt$동별_인구_1 * 100,2)
apt_info_pop_inoutcnt$동별_6_11_2_비율 <- round(apt_info_pop_inoutcnt$동별_인구_6_11_2/apt_info_pop_inoutcnt$동별_인구_2 * 100,2)
apt_info_pop_inoutcnt$동별_6_11_차이_1_비율 <- round(apt_info_pop_inoutcnt$동별_인구_6_11_차이_1/apt_info_pop_inoutcnt$동별_인구_차이_1 * 100,2)
apt_info_pop_inoutcnt$동별_6_11_차이_2_비율 <- round(apt_info_pop_inoutcnt$동별_인구_6_11_차이_2/apt_info_pop_inoutcnt$동별_인구_차이_2 * 100,2)

cat(' ','\n')
print('######## Step 5 : 데이터 셋 통합 완료 ########')
cat(' ','\n')

# Step 6-3 : 분석용 데이터셋 생성---
anal_model <- apt_info_pop_inoutcnt
# anal_model$면적67.44세대수_비율 <- round((anal_model$면적67.44세대수/anal_model$총세대수)*100,2)
# anal_model$면적67.44_80.07세대수_비율 <- round((anal_model$면적67.44_80.07세대수/anal_model$총세대수)*100,2)
# anal_model$면적80.07_86.52세대수_비율 <- round((anal_model$면적80.07_86.52세대수/anal_model$총세대수)*100,2)
# anal_model$면적86.52세대수_비율 <- round((anal_model$면적86.52세대수/anal_model$총세대수)*100,2)

if(length(colnames(anal_model)[str_detect(colnames(anal_model),"면적67.44세대수")]) != 0){
  anal_model$면적67.44세대수_비율 <- round((anal_model$면적67.44세대수/anal_model$총세대수)*100,2)
}else{
  anal_model$면적67.44세대수 = 0
  anal_model$면적67.44세대수_비율 = 0
}

if(length(colnames(anal_model)[str_detect(colnames(anal_model),"면적67.44_80.07세대수")]) != 0){
  anal_model$면적67.44_80.07세대수_비율 <- round((anal_model$면적67.44_80.07세대수/anal_model$총세대수)*100,2)
}else{
  anal_model$면적67.44_80.07세대수 = 0
  anal_model$면적67.44_80.07세대수_비율 = 0
}

if(length(colnames(anal_model)[str_detect(colnames(anal_model),"면적80.07_86.52세대수")]) != 0){
  anal_model$면적80.07_86.52세대수_비율 <- round((anal_model$면적80.07_86.52세대수/anal_model$총세대수)*100,2)
}else{
  anal_model$면적80.07_86.52세대수 = 0
  anal_model$면적80.07_86.52세대수_비율 = 0
}

if(length(colnames(anal_model)[str_detect(colnames(anal_model),"면적86.52세대수")]) != 0){
  anal_model$면적86.52세대수_비율 <- round((anal_model$면적86.52세대수/anal_model$총세대수)*100,2)
}else{
  anal_model$면적86.52세대수 = 0
  anal_model$면적86.52세대수_비율 = 0
}

anal_model$동별_6_11_차이_2_비율 <- as.integer(anal_model$동별_6_11_차이_2_비율)
anal_model <- anal_model[!is.na(anal_model$동별_6_11_차이_2_비율),]
anal_model$년도 <- anal_model$준공년도_m1

### 최종 anal_model 컬럼 select
anal_modeling <- anal_model[,c("ID", "건물명", "주소", "준공년도", "준공년도_기준_행정동명", 
                               "년도", "초등학교", "건설사",
                               "전체_전입수", "시내_전입수","시외_전입수", 
                               "전체_전출수", "시내_전출수", "시외_전출수", 
                               "전체_전입수_1", "시내_전입수_1", "시외_전입수_1", 
                               "전체_전출수_1", "시내_전출수_1", "시외_전출수_1", 
                               "전체_전입수_2", "시내_전입수_2", "시외_전입수_2", 
                               "전체_전출수_2", "시내_전출수_2", "시외_전출수_2",
                               "전체_전입수_차이_1", "시내_전입수_차이_1", "시외_전입수_차이_1", "전체_전출수_차이_1", "시내_전출수_차이_1", "시외_전출수_차이_1", 
                               "전체_전입수_차이_2", "시내_전입수_차이_2", "시외_전입수_차이_2", "전체_전출수_차이_2", "시내_전출수_차이_2", "시외_전출수_차이_2",
                               "전입_6", "전입_시내_6", "전입_시외_6", "전입_7", "전입_시내_7", "전입_시외_7", "전입_8", "전입_시내_8", "전입_시외_8", 
                               "전입_9", "전입_시내_9", "전입_시외_9", "전입_10", "전입_시내_10", "전입_시외_10", "전입_11", "전입_시내_11", "전입_시외_11", 
                               "전출_6", "전출_시내_6", "전출_시외_6", "전출_7", "전출_시내_7", "전출_시외_7", "전출_8", "전출_시내_8", "전출_시외_8", 
                               "전출_9", "전출_시내_9", "전출_시외_9", "전출_10", "전출_시내_10", "전출_시외_10", "전출_11", "전출_시내_11", "전출_시외_11", 
                               "전입_6_1", "전입_시내_6_1", "전입_시외_6_1", "전입_7_1", "전입_시내_7_1", "전입_시외_7_1", 
                               "전입_8_1", "전입_시내_8_1", "전입_시외_8_1", "전입_9_1", "전입_시내_9_1", "전입_시외_9_1", 
                               "전입_10_1", "전입_시내_10_1", "전입_시외_10_1", "전입_11_1", "전입_시내_11_1", "전입_시외_11_1",
                               "전출_6_1", "전출_시내_6_1", "전출_시외_6_1", "전출_7_1", "전출_시내_7_1", "전출_시외_7_1", 
                               "전출_8_1", "전출_시내_8_1", "전출_시외_8_1", "전출_9_1", "전출_시내_9_1", "전출_시외_9_1", 
                               "전출_10_1", "전출_시내_10_1", "전출_시외_10_1", "전출_11_1", "전출_시내_11_1", "전출_시외_11_1",
                               "전입_6_2", "전입_시내_6_2", "전입_시외_6_2", "전입_7_2", "전입_시내_7_2", "전입_시외_7_2",
                               "전입_8_2", "전입_시내_8_2", "전입_시외_8_2", "전입_9_2", "전입_시내_9_2", "전입_시외_9_2",
                               "전입_10_2", "전입_시내_10_2", "전입_시외_10_2", "전입_11_2", "전입_시내_11_2", "전입_시외_11_2",
                               "전출_6_2", "전출_시내_6_2", "전출_시외_6_2", "전출_7_2", "전출_시내_7_2", "전출_시외_7_2", 
                               "전출_8_2", "전출_시내_8_2", "전출_시외_8_2", "전출_9_2", "전출_시내_9_2", "전출_시외_9_2", 
                               "전출_10_2", "전출_시내_10_2", "전출_시외_10_2", "전출_11_2", "전출_시내_11_2", "전출_시외_11_2",
                               "전입_6_차이_1", "전입_시내_6_차이_1", "전입_시외_6_차이_1", "전입_7_차이_1", "전입_시내_7_차이_1","전입_시외_7_차이_1",
                               "전입_8_차이_1", "전입_시내_8_차이_1", "전입_시외_8_차이_1", "전입_9_차이_1", "전입_시내_9_차이_1", "전입_시외_9_차이_1",  
                               "전입_10_차이_1", "전입_시내_10_차이_1", "전입_시외_10_차이_1", "전입_11_차이_1", "전입_시내_11_차이_1", "전입_시외_11_차이_1",
                               "전출_6_차이_1", "전출_시내_6_차이_1", "전출_시외_6_차이_1", "전출_7_차이_1", "전출_시내_7_차이_1", "전출_시외_7_차이_1",  
                               "전출_8_차이_1", "전출_시내_8_차이_1", "전출_시외_8_차이_1", "전출_9_차이_1", "전출_시내_9_차이_1", "전출_시외_9_차이_1", 
                               "전출_10_차이_1", "전출_시내_10_차이_1", "전출_시외_10_차이_1", "전출_11_차이_1", "전출_시내_11_차이_1", "전출_시외_11_차이_1", 
                               "전입_6_차이_2", "전입_시내_6_차이_2", "전입_시외_6_차이_2", "전입_7_차이_2", "전입_시내_7_차이_2", "전입_시외_7_차이_2",
                               "전입_8_차이_2", "전입_시내_8_차이_2", "전입_시외_8_차이_2", "전입_9_차이_2", "전입_시내_9_차이_2", "전입_시외_9_차이_2",
                               "전입_10_차이_2", "전입_시내_10_차이_2", "전입_시외_10_차이_2", "전입_11_차이_2", "전입_시내_11_차이_2", "전입_시외_11_차이_2",
                               "전출_6_차이_2", "전출_시내_6_차이_2", "전출_시외_6_차이_2", "전출_7_차이_2", "전출_시내_7_차이_2", "전출_시외_7_차이_2",
                               "전출_8_차이_2", "전출_시내_8_차이_2", "전출_시외_8_차이_2", "전출_9_차이_2", "전출_시내_9_차이_2", "전출_시외_9_차이_2",  
                               "전출_10_차이_2", "전출_시내_10_차이_2", "전출_시외_10_차이_2", "전출_11_차이_2", "전출_시내_11_차이_2", "전출_시외_11_차이_2",
                               "전입_전체_6_11_합", "전입_시내_6_11_합", "전입_시외_6_11_합",
                               "전출_전체_6_11_합", "전출_시내_6_11_합", "전출_시외_6_11_합",
                               "전입_전체_6_11_합_1", "전입_시내_6_11_합_1", "전입_시외_6_11_합_1",
                               "전출_전체_6_11_합_1", "전출_시내_6_11_합_1", "전출_시외_6_11_합_1",
                               "전입_전체_6_11_합_2", "전입_시내_6_11_합_2", "전입_시외_6_11_합_2",
                               "전출_전체_6_11_합_2", "전출_시내_6_11_합_2", "전출_시외_6_11_합_2",
                               "전입_전체_6_11_합_차이_1", "전입_시내_6_11_합_차이_1", "전입_시외_6_11_합_차이_1",
                               "전출_전체_6_11_합_차이_1", "전출_시내_6_11_합_차이_1", "전출_시외_6_11_합_차이_1",
                               "전입_전체_6_11_합_차이_2", "전입_시내_6_11_합_차이_2", "전입_시외_6_11_합_차이_2",
                               "전출_전체_6_11_합_차이_2", "전출_시내_6_11_합_차이_2", "전출_시외_6_11_합_차이_2",
                               "동별_인구", "동별_인구_6_11", "동별_6_11_비율",
                               "동별_인구_1", "동별_인구_6_11_1", "동별_6_11_1_비율", 
                               "동별_인구_2", "동별_인구_6_11_2", "동별_6_11_2_비율", 
                               "동별_인구_차이_1", "동별_인구_6_11_차이_1", "동별_6_11_차이_1_비율", 
                               "동별_인구_차이_2", "동별_인구_6_11_차이_2", "동별_6_11_차이_2_비율",
                               "초등학교와의거리", "세대당주차대수", "용적률", "건폐율", "평균면적",
                               "총세대수", "면적67.44세대수", "면적67.44_80.07세대수", "면적80.07_86.52세대수", "면적86.52세대수", 
                               "면적67.44세대수_비율", "면적67.44_80.07세대수_비율", "면적80.07_86.52세대수_비율", "면적86.52세대수_비율",
                               "동수","최저층","최고층","평균방수","평균욕실수",
                               "종합병원까지의거리","대형마트까지의거리","도서관까지의거리","중학교까지의거리",
                               "난방방식","난방종류","현관구조","마트","편의점","어린이집","헬스장","실내골프장","배드민턴장","무인택배함","독서실","카페","농구장", "유치원"
)]
cat(' ','\n')
print('######## Step 6 : 분석용 데이터셋 생성 완료 ########')
cat(' ','\n')
print('######## ★★★★★ 데이터 전처리 (Data Preprocessing) 완료 ★★★★★ ########')
cat(' ','\n')
