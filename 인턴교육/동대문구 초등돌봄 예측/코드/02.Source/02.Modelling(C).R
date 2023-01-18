#####################################################################################################
## 03. 수요예측분석 (Modelling)
#####################################################################################################

# Packages
# install.packages("e1071")
# install.packages("kernlab")
# install.packages("caret")
# install.packages("randomForest")

### 라이브러리 ###
options(scipen = 6)
library(e1071)
library(kernlab)
library(caret)
library(randomForest)

## 전처리된 모델 형 변환##

anal_modeling$준공년도 <- as.factor(anal_modeling$준공년도)
anal_modeling$초등학교 <- as.factor(anal_modeling$초등학교)
anal_modeling$건설사 <- as.factor(anal_modeling$건설사)
anal_modeling$난방방식 <- as.factor(anal_modeling$난방방식)
anal_modeling$난방종류 <- as.factor(anal_modeling$난방종류)
anal_modeling$현관구조 <- as.factor(anal_modeling$현관구조)
anal_modeling$마트 <- as.factor(anal_modeling$마트)
anal_modeling$편의점 <- as.factor(anal_modeling$편의점)
anal_modeling$어린이집 <- as.factor(anal_modeling$어린이집)
anal_modeling$헬스장 <- as.factor(anal_modeling$헬스장)
anal_modeling$실내골프장 <- as.factor(anal_modeling$실내골프장)
anal_modeling$배드민턴장 <- as.factor(anal_modeling$배드민턴장)
anal_modeling$무인택배함 <- as.factor(anal_modeling$무인택배함)
anal_modeling$독서실 <- as.factor(anal_modeling$독서실)
anal_modeling$카페 <- as.factor(anal_modeling$카페)
anal_modeling$농구장 <- as.factor(anal_modeling$농구장)
anal_modeling$유치원 <- as.factor(anal_modeling$유치원)
anal_modeling$용적률 <- as.integer(anal_modeling$용적률)
anal_modeling$동별_6_11_차이_2_비율 <- as.integer(anal_modeling$동별_6_11_차이_2_비율)


### 03-1. 저장된 모델 불러오기 및 예측
load("C:/Users/user/Desktop/초등돌봄수요예측/03.groupC/02.Source/C.model.RData")
model.rf.hat.check <- round(predict(C.model, anal_modeling))

### 03-2. 면적별 맞벌이 비율 데이터 불러오기
load("C:/Users/user/Desktop/초등돌봄수요예측/03.groupC/01.Preprocessing/03.pre_data3/C_area_p.RData")
area_p <- C_area_p

result_data<-anal_modeling[,c(2,3,268,269)]
result_data$맞벌이비율 <- ifelse(result_data$평균면적 < area_p[1,1], area_p[1,2], 0)
result_data$맞벌이비율 <- ifelse(result_data$평균면적 >= area_p[1,1] & result_data$평균면적 < area_p[2,1], area_p[2,2], result_data$맞벌이비율)
result_data$맞벌이비율 <- ifelse(result_data$평균면적 >= area_p[2,1] & result_data$평균면적 < area_p[3,1], area_p[3,2], result_data$맞벌이비율)
result_data$맞벌이비율 <- ifelse(result_data$평균면적 >= area_p[3,1], area_p[4,2], result_data$맞벌이비율)

### 03-3. 면적별 맞벌이 비율과 초등예측값 결합
C.2019.result<-cbind(result_data,model.rf.hat.check)

names(C.2019.result) <- c("주택단지명","주소","평균면적","총세대수","맞벌이비율","초등수예측")

### 03-4. 총 세대수 대비 예측 범위 적용
{
  #군집C
  hat_Q4<-0.3225
  hat_Q1<-0.1094
  
  C.2019.result$초총Q4<-hat_Q4*C.2019.result$총세대수
  C.2019.result$초총Q1<-hat_Q1*C.2019.result$총세대수
  
  for( i in 1:length(C.2019.result$초총Q4)) {
    i=1
    if(C.2019.result$초등수예측[i]>=C.2019.result$초총Q4[i]) {
      C.2019.result$초등수예측[i]<-C.2019.result$초총Q4[i] }
    else {C.2019.result$초등수예측[i]<-C.2019.result$초등수예측[i]}
  }
  
  for( i in 1:length(C.2019.result$초총Q1)) {
    if(C.2019.result$초등수예측[i]<=0) {
      C.2019.result$초등수예측[i]<-0} 
    else {C.2019.result$초등수예측[i]<-C.2019.result$초등수예측[i]}
  }
  
  for( i in 1:length(C.2019.result$초총Q1)) {
    if(C.2019.result$초등수예측[i]<=C.2019.result$초총Q1[i]) {
      C.2019.result$초등수예측[i]<-C.2019.result$초총Q1[i]} 
    else {C.2019.result$초등수예측[i]<-C.2019.result$초등수예측[i]}
  }
  
}

### 03-5. 면적별 맞벌이 비율과 곱하여 최종 돌봄예측초등수 산출
C.2019.result$돌봄예측초등수<-as.numeric(C.2019.result$초등수예측*C.2019.result$맞벌이비율)

### 03-6. 최종 예측결과 저장
#단지명, 주소, 총세대수, 돌봄예측초등수 컬럼만 선택
result_data<-C.2019.result[,c(1,2,4,9)]
result_data$돌봄예측초등수 <- as.numeric(result_data$돌봄예측초등수)
result_data$돌봄예측초등수<-round(result_data$돌봄예측초등수)

names(result_data) <- c("주택단지명","주소","총세대수","돌봄예측초등수")

imagedir <- paste("C:/Users/user/Desktop/초등돌봄수요예측/03.groupC/03.Result/", substr(Sys.Date(), 1, 4), sep="")
imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
imagedir5 <- paste(imagedir, "군집C 예측결과", sep="_")
write.csv(result_data, file = paste(imagedir5, ".csv", sep=""), fileEncoding = 'CP949')

print('######## ★★★★★ 수요예측분석 (Modelling) 완료 ★★★★★ ########')
