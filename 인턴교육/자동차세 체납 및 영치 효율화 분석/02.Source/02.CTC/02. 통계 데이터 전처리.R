######################################################################################
                                  # 통계 데이터 전처리 #
######################################################################################

################################
# 1. 주차장 관련 데이터셋 구축 #
################################

# 데이터 불러오기
doro_500m_buff_500m <- st_read("../03.Output/doro_500m_buff_500m.shp", options="ENCODING=cp949") %>% st_transform(5179)
parking_lot <- st_read("../03.Output/07_parking_lot.shp", options="ENCODING=cp949") %>% st_transform(5179)

# 도로, 버퍼별 주차장 수, 주차면 수 집계
park_lot <- st_join(doro_500m_buff_500m, parking_lot, left=TRUE) %>% group_by(split_id) %>% 
  summarise(park_lot = as.integer(sum(park_lot)), park_n = ifelse(!is.na(park_lot), n(), NA)) %>% 
  as.data.frame %>% select(park_lot, park_n)
doro_500m_buff_500m <- doro_500m_buff_500m %>% bind_cols(park_lot) %>% as.data.frame %>% select(-geometry)
write.csv(doro_500m_buff_500m, "../03.Output/doro.csv", row.names=FALSE, na="")



################################
# 2. 통계 모델링 데이터셋 생성 #
################################

# 데이터 불러오기
youngchi <- read.csv('영치차량내역서(수정본).csv', encoding='cp949', fileEncoding = 'cp949')
chenap <- read.csv('체납자주소좌표.csv', encoding='cp949', fileEncoding = 'cp949')
carreg <- read.csv('차량등록현황_좌표추가.csv', encoding='cp949', fileEncoding = 'cp949')
cctv_fix <- read.csv('04_cctv_fix.csv', encoding='cp949', fileEncoding = 'cp949')
#cctv_fix_alarm <- read.csv('04-1_cctv_fix_alarm.csv', encoding='cp949', fileEncoding = 'cp949')
#cctv_fix_loc <- read.csv('04-2_cctv_fix_loc.csv', encoding='cp949', fileEncoding = 'cp949')
#cctv_road <- read.csv('05_cctv_road.csv', encoding='cp949', fileEncoding = 'cp949')
#cctv_road_loc <- read.csv('05-1_cctv_road_loc.csv', encoding='cp949', fileEncoding = 'cp949')
cctv_mov <- read.csv('06_cctv_mov.csv', encoding='cp949', fileEncoding = 'cp949')
yh <- read.csv('time_range.csv', encoding='cp949', fileEncoding = 'cp949')
names(yh) <- c("yh", "min_yc", "max_yc", "max_cn", "min_mov", "max_mov")

# 중요 컬럼 트리밍 처리
youngchi$car_num <- trimws(youngchi$car_num)
chenap$car_num <- trimws(chenap$car_num)
carreg$car_num <- trimws(carreg$car_num)
cctv_mov$car_num <- trimws(cctv_mov$car_num)
cctv_fix$car_num <- trimws(cctv_fix$car_num)
cctv_fix_alarm$car_num <- trimws(cctv_fix_alarm$car_num)
cctv_road$car_num <- trimws(cctv_road$car_num)
youngchi$ymd <- trimws(youngchi$ymd)
chenap$ymd <- trimws(chenap$ymd)
cctv_mov$t <- trimws(cctv_mov$t)
cctv_fix$t <- trimws(cctv_fix$t)
cctv_fix_alarm$t <- trimws(cctv_fix_alarm$t)



##### 예측시기값 부여
# 영치 데이터
youngchi$yh <- NA
ymd = as.integer(as.Date(youngchi$yungchi_date))
for (row in 1:nrow(yh)) {
  youngchi$yh <- ifelse(ymd >= as.integer(as.Date(yh[row, 'min_yc'])) 
                        & ymd <= as.integer(as.Date(yh[row, 'max_yc'])), row, youngchi$yh)
}

# 체납 데이터
chenap$yh <- NA
ymd = as.integer(as.Date(chenap$yungchi_da))
for (row in 1:nrow(yh)) {
  chenap$yh <- ifelse(ymd <= as.integer(as.Date(yh[row, 'max_cn'])), 
                      ifelse(is.na(chenap$yh), row, chenap$yh), NA)
}

# 이동형 데이터
cctv_mov$yh <- NA
t = unclass(as.POSIXct(cctv_mov$단속일시))
for (row in 1:nrow(yh)) {
  cctv_mov$yh <- ifelse(t >= unclass(as.POSIXct(yh[row, 'min_mov'])) &
                          t <= unclass(as.POSIXct(yh[row, 'max_mov'])), row, cctv_mov$yh)
}



##### 체납 + 영치 믹스업하여 데이터셋 생성 (cnyc)
chenap_yh <- chenap[!is.na(chenap$yh),]
youngchi_yh <- youngchi[!is.na(youngchi$yh),]
cn_join_yc <- full_join(chenap_yh, youngchi_yh, 'id')
cnyc <- data.frame(
  car_num=cn_join_yc$id,
  adr_x=case_when(!is.na(cn_join_yc$lng) ~ cn_join_yc$lng, TRUE ~ cn_join_yc$X),
  adr_y=case_when(!is.na(cn_join_yc$lat) ~ cn_join_yc$lat, TRUE ~ cn_join_yc$Y),
  yh1=case_when(coalesce(cn_join_yc$yh.x, 0) >= 1 | cn_join_yc$yh.y == 1 ~ 1, TRUE ~ 0),
  yh2=case_when(coalesce(cn_join_yc$yh.x, 0) >= 2 | cn_join_yc$yh.y == 2 ~ 1, TRUE ~ 0),
  # yh3=case_when(coalesce(cn_join_yc$yh.x, 0) >= 3 | cn_join_yc$yh.y == 3 ~ 1, TRUE ~ 0),
  man=case_when(cn_join_yc$division.x == '남' | cn_join_yc$division.y == '남' ~ 1, TRUE ~ 0),
  woman=case_when(cn_join_yc$division.x == '여' | cn_join_yc$division.y == '여' ~ 1, TRUE ~ 0))


#cnyc_join_cr = na.omit(cnyc_join_cr)
cnyc_join_cr = unique(cnyc_join_cr)
cnyc_join_cr = cnyc_join_cr %>% distinct(id, .keep_all = TRUE)
cnyc_join_cr = cnyc_join_cr %>% distinct(car_num, .keep_all = TRUE)

# 차종정보 결합
cnyc_join_cr <- left_join(cnyc, carreg, 'adr_y')
cnyc <- cnyc %>% mutate(sy=case_when(cnyc_join_cr$cat == '승용' ~ 1, TRUE ~ 0),
                        sh=case_when(cnyc_join_cr$cat == '승합' ~ 1, TRUE ~ 0),
                        ts=case_when(cnyc_join_cr$cat == '특수' ~ 1, TRUE ~ 0),
                        hm=case_when(cnyc_join_cr$cat == '화물' ~ 1, TRUE ~ 0))



##### 이동형 단속내역에서 예측시기별 체납자여부 값 및 오전/오후 컬럼 부여 (mov)
mov_join_cnyc <- left_join(cctv_mov, cnyc, 'car_num')
mov <- data.frame(id=mov_join_cnyc$id,
                  car_num=mov_join_cnyc$car_num,
                  yh=mov_join_cnyc$yh,
                  cn_yn=case_when(
                      (mov_join_cnyc$yh == 1 & mov_join_cnyc$yh1 == 1) | 
                      (mov_join_cnyc$yh == 2 & mov_join_cnyc$yh2 == 1) | 
                      (mov_join_cnyc$yh == 3 & mov_join_cnyc$yh3 == 1) ~ 1,
                    TRUE ~ 0),
                  h1=ifelse(as.POSIXlt(mov_join_cnyc$t)$hour < 12, 1, 0),
                  h2=ifelse(as.POSIXlt(mov_join_cnyc$t)$hour >= 12, 1, 0),
                  x=mov_join_cnyc$x,
                  y=mov_join_cnyc$y)
mov <- mov %>% filter(!is.na(mov$yh))



# 고정형 + 고정형 알리미 통합(1시간 이내 중복 제거)
fix_union <- union_all(cctv_fix[,c("car_num", "t", "cctv_id")], 
                       cctv_fix_alarm[,c("car_num", "t", "cctv_id")])
fix_except <- inner_join(cctv_fix, cctv_fix_alarm, c('car_num', 'cctv_id'))
fix_except <- subset(fix_except, 
              select= c('car_num', 't.y', 'cctv_id'),
              subset=(as.POSIXlt(fix_except$t.x)$hour - as.POSIXlt(fix_except$t.y)$hour == 0)) %>%
              rename("t"="t.y")
fix_union <- anti_join(fix_union, fix_except, 'car_num') %>% mutate(id=row_number()) %>% 
              select(id, car_num, t, cctv_id)

# 고정형, 도로방범 데이터 오전오후값 부여
fix_union$h <- ifelse(as.POSIXlt(fix_union$t)$hour < 12, 1, 2)
cctv_road$h <- ifelse(as.POSIXlt(cctv_road$t)$hour < 12, 1, 2)



##### 분석용 고정형(불법주정차) 집계 데이터셋 생성 (fix)
fix_join_chenap <- left_join(fix_union, chenap, 'car_num')
fix_join_chenap <- fix_join_chenap %>% mutate(
  h1=case_when(fix_join_chenap$h == 1 ~ 1, TRUE ~ 0),
  h1_cn=case_when(fix_join_chenap$h == 1 & !is.na(fix_join_chenap$id.y) ~ 1, TRUE ~ 0),
  h2=case_when(fix_join_chenap$h == 2 ~ 1, TRUE ~ 0),
  h2_cn=case_when(fix_join_chenap$h == 2 & !is.na(fix_join_chenap$id.y) ~ 1, TRUE ~ 0))

group_by_fix_join_chenap <- fix_join_chenap %>% group_by(cctv_id) %>% 
  summarise(h1_total = sum(h1), h1_cn = sum(h1_cn), h2_total = sum(h2), h2_cn = sum(h2_cn)) %>% 
  subset(h1_total > 0 & h2_total > 0)

x <- inner_join(group_by_fix_join_chenap, cctv_fix_loc, 'cctv_id')
fix <- data.frame(cctv_id = x$cctv_id,
                  h1_total = x$h1_total,
                  h1_cn = x$h1_cn,
                  h1_cn_r = x$h1_cn / x$h1_total,
                  h2_total = x$h2_total,
                  h2_cn = x$h2_cn,
                  h2_cn_r = x$h2_cn / x$h2_total,
                  x = x$x,
                  y = x$y)



##### 분석용 고정형(도로방범) 집계 데이터셋 생성 (road)
road_join_cnyc <- left_join(cctv_road, cnyc, 'car_num')
road_join_cnyc <- road_join_cnyc %>% mutate(
  h1=case_when(road_join_cnyc$h == 1 ~ 1, TRUE ~ 0),
  h1_cn=case_when(road_join_cnyc$h == 1 & !is.na(road_join_cnyc$adr_x) ~ 1, TRUE ~ 0),
  h2=case_when(road_join_cnyc$h == 2 ~ 1, TRUE ~ 0),
  h2_cn=case_when(road_join_cnyc$h == 2 & !is.na(road_join_cnyc$adr_x) ~ 1, TRUE ~ 0)
)

group_by_road_join_cnyc <- road_join_cnyc %>% group_by(cctv_id) %>% 
  summarise(h1_total = sum(h1), h1_cn = sum(h1_cn), h2_total = sum(h2), h2_cn = sum(h2_cn))
group_by_road_join_cnyc <- subset(group_by_road_join_cnyc, h1_total > 100 & h2_total > 100)

x <- inner_join(group_by_road_join_cnyc, cctv_road_loc, 'cctv_id')
road <- data.frame(cctv_id = x$cctv_id,
                   h1_total = x$h1_total,
                   h1_cn = x$h1_cn,
                   h1_cn_r = x$h1_cn / x$h1_total,
                   h2_total = x$h2_total,
                   h2_cn = x$h2_cn,
                   h2_cn_r = x$h2_cn / x$h2_total,
                   x = x$x,
                   y = x$y)



# 직장주소 (Optional) (work)
work <- data.frame(car_num = chenap_yh$car_num,
                   work_x = chenap_yh$work_x,
                   work_y = chenap_yh$work_y,
                   yh1 = case_when(coalesce(chenap_yh$yh, 0) >= 1 ~ 1, TRUE ~ 0),
                   yh2 = case_when(coalesce(chenap_yh$yh, 0) >= 2 ~ 1, TRUE ~ 0),
                   yh3 = case_when(coalesce(chenap_yh$yh, 0) >= 3 ~ 1, TRUE ~ 0))
work <- subset(work, !is.na(work_x))

# 체납영치 결과테이블에서 좌표 없는 값 삭제
cnyc <- subset(cnyc, !is.na(adr_x))

# 영치 데이터 (yc)
yc <- youngchi %>% select(car_num, yh, loc_x, loc_y) %>% filter(!is.na(loc_x) & !is.na(yh))

# 결과 파일 출력
write.csv(cnyc, "../03.Output/cnyc.csv", row.names=FALSE)
write.csv(mov, "../03.Output/mov.csv", row.names=FALSE)
write.csv(fix, "../03.Output/fix.csv", row.names=FALSE)
write.csv(road, "../03.Output/road.csv", row.names=FALSE)
write.csv(work, "../03.Output/work.csv", row.names=FALSE)
write.csv(yc, "../03.Output/yc.csv", row.names=FALSE)