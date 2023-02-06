######################################################################################
                            # 공간데이터 전처리 구축 #
######################################################################################


################################
# 1. 행정동 집계 데이터 전처리 #
################################

# 데이터 불러오기
dong <- st_read("emd.shp", options="ENCODING=cp949") %>% st_transform(5179)
youngchi_adr <- st_read("../03.Output/01_youngchi_adr.shp", options="ENCODING=cp949")
chenap_adr <- st_read("../03.Output/02_chenap_adr.shp", options="ENCODING=cp949")
carreg_adr <- st_read("../03.Output/03_carreg_adr.shp", options="ENCODING=cp949")


# 집계 데이터 필드 추가 
dong$영치건수 <- lengths(st_intersects(dong, youngchi_adr))
dong$체납건수 <- lengths(st_intersects(dong, chenap_adr))
dong$등록건수 <- lengths(st_intersects(dong, carreg_adr))
dong <- dong %>% rename("adm_dr_nm" = "ADM_DR_NM")
meanprice <- st_join(dong, chenap_adr, left=TRUE) %>% group_by(adm_dr_nm) %>% 
  summarise(meanprice = as.integer(mean(price))) %>% as.data.frame %>% select(meanprice)
dong <- dong %>% bind_cols(meanprice)
hangjung <- dong %>% as.data.frame %>% select(adm_dr_nm, 영치건수, 체납건수, 등록건수)

# 집계 파일 생성
st_write(dong, "../03.Output/DONG_집계.shp", layer_options="ENCODING=cp949")
write.csv(hangjung, "../03.Output/행정동별_집계.csv", row.names=FALSE, na="")



######################################################
# 2. 영치장소 및 차량사용본거지에 대해 행정동명 결합 #
######################################################

# 데이터 불러오기
dong <- st_read("emd.shp", options="ENCODING=cp949") %>% st_transform(5179) %>% rename("emd_kor_nm" = "EMD_KOR_NM")
youngchi_loc <- st_read("../03.Output/01_youngchi_loc.shp", options="ENCODING=cp949")
youngchi_adr <- st_read("../03.Output/01_youngchi_adr.shp", options="ENCODING=cp949")

dong_joined_01 = st_join(st_filter(youngchi_loc, dong, left=TRUE), dong, left=TRUE)
dong_joined_01_1 = st_join(st_filter(youngchi_adr, dong, left=TRUE), dong, left=TRUE)
dong_joined_01_1 <- dong_joined_01_1 %>% select(id, emd_kor_nm) %>% rename(f_emd_kor_nm = emd_kor_nm)
adm_dr_nm <- dong_joined_01 %>% as.data.frame() %>% select(id, emd_kor_nm)
f_adm_dr_nm <- dong_joined_01_1 %>% as.data.frame() %>% select(id, f_emd_kor_nm)
youngchi_hangjung <- merge(adm_dr_nm, f_adm_dr_nm, key='id') %>% select(emd_kor_nm, f_emd_kor_nm)
dong_hangjung = dong %>% as.data.frame() %>% select(emd_kor_nm)

# 파일 생성
write.csv(dong_hangjung, '../03.Output/00_행정동만.csv', row.names=FALSE, na="", fileEncoding = "cp949")
write.csv(youngchi_hangjung, "../03.Output/01_영치_행정동만.csv", row.names=FALSE, na="", fileEncoding = "cp949")

##################################
# 3. 표준화 주차장 데이터셋 생성 #
##################################

# 표제부 파일 불러오기
jucha <- as.data.frame(readxl::read_excel("표제부 조회.xlsx", sheet="표제부 조회", 
                                          col_names=TRUE, skip=4))

# 표준화 주차장 데이터셋 전처리
jucha$총주차면수 <- rowSums(sapply(subset(jucha, select=c('옥내기계식대수(대)', 
                                                     '옥외기계식대수(대)', 
                                                     '옥내자주식대수(대)', 
                                                     '옥외자주식대수(대)')), as.numeric))
jucha <- jucha %>% filter(총주차면수 != 0) %>% arrange(대지위치, desc(총주차면수))
jucha$위치별선택 <- ifelse(jucha$대지위치 == lag(jucha$대지위치, 1, default='0'), 0, 1)
jucha <- jucha %>% filter(위치별선택 == 1)

# 작업 후 지오코딩 필수
# 지오코딩 후 파일명은 07_parking_lot.csv
write.csv(jucha, '../03.Output/pyojae_park.csv', row.names=FALSE, na="", fileEncoding = "cp949")

# 지오코딩 이후 실행
pyojae_park <- read.csv("../03.Output/pyojae_park.csv", encoding='cp949', fileEncoding = "CP949") %>% 
  select('건물명', '대지위치', 'X', 'Y', '총주차면수') %>%
  rename(nm=건물명, adr=대지위치, x=X, y=Y, park_lot=총주차면수)

jucha_park <- read.csv("../03.Output/jucha_park.csv", encoding="cp949", fileEncoding = "CP949") %>%
  select('주차장명', '소재지지번주소', '경도', '위도', '주차구획수') %>%
  rename(nm=주차장명, adr=소재지지번주소, x=경도, y=위도, park_lot=주차구획수)


parking_lot <- data.frame(nm = character(0), 
                          adr = character(0), 
                          x = numeric(0), 
                          y = numeric(0), 
                          park_lot = numeric(0))

parking_lot <- parking_lot %>% bind_rows(pyojae_park) %>% bind_rows(jucha_park) %>% mutate(id = row_number()) %>% 
  select(id, nm, adr, x, y, park_lot)

write.csv(parking_lot, "../03.Output/07_parking_lot.csv", row.names=FALSE, na="", fileEncoding = "cp949")



#######################
# 4. 주소 좌표화 작업 #
#######################

# GeocodingTool 및 혜안을 통해서 좌표화 작업 시행



###########################
# 5. 공간데이터 변환 작업 #
###########################

# 표준화 주차장 데이터셋 공간데이터 변환
parking_lot <- read.csv('../03.Output/07_parking_lot.csv', encoding='cp949', fileEncoding = "cp949") %>% filter(!is.na(x) & !is.na(y)) %>% 
  st_as_sf(coords=c('x', 'y'), crs=4326, remove=FALSE) %>% st_transform(5179)
st_write(parking_lot, "../03.Output/07_parking_lot.shp", layer_options="ENCODING=cp949", fileEncoding = "cp949")



################################
# 6. 표준화 데이터셋 생성 확인 #
################################

# 01_youngchi_loc.shp, 01_youngchi_adr.shp
# 02_chenap_adr.shp, 02_chenap_work.shp
# 03_carreg_adr.shp
# 07_parking_lot.shp



##########################
# 7. 단위도로(500m) 생성 #
##########################

# 데이터 불러오기
TL_SPRD_MANAGE <- st_read("TL_SPRD_MANAGE.shp", options="ENCODING=cp949", crs = 5179) %>% 
  st_transform(5179) %>% select(RDS_MAN_NO, ROAD_BT)

long_doro <- TL_SPRD_MANAGE %>% filter(as.numeric(st_length(TL_SPRD_MANAGE)) > 500) %>% 
  st_cast("LINESTRING")
solo_doro <- TL_SPRD_MANAGE %>% filter(as.numeric(st_length(TL_SPRD_MANAGE)) <= 500) %>% 
  st_cast("LINESTRING")

doro <- long_doro %>% filter(is.na(geometry)) %>% mutate(split_id=NA)
lines = 1 / as.integer(as.integer(st_length(long_doro) / 500) + 1)

# 시간이 오래 소요될 수 있음
# 500m 이상 도로에 대해 단위도로 생성
tmp = list()
n = 1
for(i in 1:nrow(long_doro)) {
  k = 0 
  while(k < 0.9999) {
    tmp[[n]] = st_linesubstring(long_doro[i,], k, k+lines[i], lines[i])
    k = k + lines[i]
    n = n + 1
  }
}

# 단위도로 (500m)
doro <- rbindlist(tmp) %>% st_as_sf(crs=5179) %>% st_segmentize(units::set_units(1, "m")) %>% 
  bind_rows(solo_doro) %>% mutate(split_id=row_number())
st_write(doro, "../03.Output/doro_500m.shp", layer_options="ENCODING=cp949")

# 단위도로별 중심점
doro_centro <- st_linesubstring(doro, 0.5, 0.5) %>% st_cast("POINT")
st_write(doro_centro, "../03.Output/doro_500m_centro.shp", layer_options="ENCODING=cp949")

# 단위도로별 중심점 버퍼
doro_buffer <- st_buffer(doro_centro, 500, 20)
st_write(doro_buffer, "../03.Output/doro_500m_buff_500m.shp", layer_options="ENCODING=cp949")
