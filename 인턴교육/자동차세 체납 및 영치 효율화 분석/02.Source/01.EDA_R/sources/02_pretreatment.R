## 각종 타입 지정
youngchi$영치일 <- as.Date(youngchi$영치일)
## chenap$ymd <- as.Date(chenap$ymd)

## 자주 사용하는 기본변수 산출 저장
n_youngchi <- nrow(youngchi) #총영치수
n_chenap <- nrow(chenap) #총체납수
## n_car <- nrow(carreg) #차량등록수
## chenap_rate <- n_chenap / n_car #평균체납율


## 날짜에서 상/하반기 반환하는 함수
get_halfyear <- function(ymd) {
  x <- ceiling(as.integer(format(ymd, "%m"))/6)
  return(ifelse(x == 1, "상반기", "하반기"))
}

print("02_pretreatment: 데이터 기본 처리 완료")
