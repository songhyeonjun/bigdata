# 환경 초기화
rm(list = ls())

# 패키지 부착
library(sqldf)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)

## 파일 읽어오기
youngchi <- data.frame(read.csv("./data/영치차량내역서(수정본).csv", na.strings = '', fileEncoding = "CP949", encoding = "UTF-8")) #영치
chenap <- data.frame(read.csv("./data/자동차세 체납자료(동대문구만).csv", na.strings = '', fileEncoding = "CP949", encoding = "UTF-8")) #체납
carreg <- data.frame(read.csv("./data/차량등록현황(202201).csv", na.strings = '', fileEncoding = "CP949", encoding = "UTF-8")) #차량등록

print("01_load: 패키지 부착 및 데이터 로드 완료")
