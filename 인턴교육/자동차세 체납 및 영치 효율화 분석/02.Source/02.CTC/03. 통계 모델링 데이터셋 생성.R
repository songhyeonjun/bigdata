######################################################################################
                              # 통계 모델링 데이터셋 생성 #
######################################################################################

########################################
# 1. 영치 장소(모델 Y값) 데이터셋 구축 #
########################################

# 데이터 불러오기
doro_500m <- st_read("../03.Output/doro_500m.shp", options="ENCODING=cp949", crs=5179)
yc <- read.csv("../03.Output/yc.csv", encoding='cp949')
if (max(yc$loc_y, na.rm=TRUE) > 39) {
  yc <- yc %>% st_as_sf(coords=c('loc_x', 'loc_y'), crs=5179, remove=FALSE)
} else {
  yc <- yc %>% st_as_sf(coords=c('loc_x', 'loc_y'), crs=4326, remove=FALSE) %>% st_transform(5179)
}

yc_doro <- st_join(yc, doro_500m, join=st_nearest_feature)
yc_doro <- yc_doro %>% group_by(split_id, yh) %>% tally() %>% as.data.frame() %>% 
  select(-geometry) %>% rename(yc=n)

write.csv(yc_doro, "../03.Output/yc.csv", row.names=FALSE)



#####################################
# 2. 도로 주변 체납자 데이터셋 구축 #
#####################################

# 데이터 불러오기
doro_500m_buff_500m <- st_read("../03.Output/doro_500m_buff_500m.shp", options="ENCODING=cp949", crs=5179)
cnyc <- read.csv("../03.Output/cnyc.csv", encoding='cp949')
if (max(cnyc$adr_y, na.rm=TRUE) > 39) {
  cnyc <- cnyc %>% st_as_sf(coords=c('adr_x', 'adr_y'), crs=5179, remove=FALSE)
} else {
  cnyc <- cnyc %>% st_as_sf(coords=c('adr_x', 'adr_y'), crs=4326, remove=FALSE) %>% st_transform(5179)
}

# cnyc$yh1 == 1
cnyc_yh1 <- cnyc %>% filter(yh1 == 1) %>% select(-c(adr_x, adr_y, yh2, yh3))
cnyc_yh1 <- st_join(doro_500m_buff_500m, cnyc_yh1, left=TRUE) %>% group_by(split_id) %>% 
  summarise(m500_cn_n=sum(yh1), 
            m500_man=sum(man), 
            m500_woman=sum(woman), 
            m500_sy=sum(sy), 
            m500_sh=sum(sh), 
            m500_ts=sum(ts), 
            m500_hm=sum(hm)) %>% as.data.frame %>% select(-geometry)
write.csv(cnyc_yh1, '../03.Output/yh1_cn.csv', row.names=FALSE, na="")

# cnyc$yh2 == 1
cnyc_yh2 <- cnyc %>% filter(yh2 == 1) %>% select(-c(adr_x, adr_y, yh1, yh3))
cnyc_yh2 <- st_join(doro_500m_buff_500m, cnyc_yh2, left=TRUE) %>% group_by(split_id) %>% 
  summarise(m500_cn_n=sum(yh2),
            m500_man=sum(man),
            m500_woman=sum(woman), 
            m500_sy=sum(sy), 
            m500_sh=sum(sh), 
            m500_ts=sum(ts), 
            m500_hm=sum(hm)) %>% as.data.frame %>% select(-geometry)
write.csv(cnyc_yh2, '../03.Output/yh2_cn.csv', row.names=FALSE, na="")

# cnyc$yh3 == 1
cnyc_yh3 <- cnyc %>% filter(yh3 == 1) %>% select(-c(adr_x, adr_y, yh1, yh2))
cnyc_yh3 <- st_join(doro_500m_buff_500m, cnyc_yh3, left=TRUE) %>% group_by(split_id) %>%
  summarise(m500_cn_n=sum(yh3),
            m500_man=sum(man),
            m500_woman=sum(woman), 
            m500_sy=sum(sy), 
            m500_sh=sum(sh), 
            m500_ts=sum(ts),
            m500_hm=sum(hm)) %>% as.data.frame %>% select(-geometry)
write.csv(cnyc_yh3, '../03.Output/yh3_cn.csv', row.names=FALSE, na="")



#########################################
# 3. 이동형 단속내역 관련 데이터셋 구축 #
#########################################

# 데이터 불러오기
doro_500m_buff_500m <- st_read("../03.Output/doro_500m_buff_500m.shp", options="ENCODING=cp949", crs=5179)
mov <- read.csv("../03.Output/mov.csv", encoding='cp949')
if (max(mov$y, na.rm=TRUE) > 39) {
  mov <- mov %>% st_as_sf(coords=c('x', 'y'), crs=5179, remove=FALSE)
} else {
  mov <- mov %>% st_as_sf(coords=c('x', 'y'), crs=4326, remove=FALSE) %>% st_transform(5179)
}

# mov$yh == 1
mov_yh1 <- mov %>% filter(yh == 1) %>% select(-c(x, y, yh))
mov_yh1 <- st_join(doro_500m_buff_500m, mov_yh1, left=TRUE) %>% group_by(split_id) %>% 
  summarise(m500_mov_total=sum(!is.na(id)), 
            m500_mov_cn=sum(cn_yn), 
            m500_mov_cn_h1=sum(h1), 
            m500_mov_cn_h2=sum(h2)) %>% as.data.frame %>% select(-geometry)
mov_yh1$m500_mov_total <- ifelse(mov_yh1$m500_mov_total, mov_yh1$m500_mov_total, NA)
write.csv(mov_yh1, '../03.Output/yh1_mov.csv', row.names=FALSE, na="")

# mov$yh == 2
mov_yh2 <- mov %>% filter(yh == 2) %>% select(-c(x, y, yh))
mov_yh2 <- st_join(doro_500m_buff_500m, mov_yh2, left=TRUE) %>% group_by(split_id) %>% 
  summarise(m500_mov_total=sum(!is.na(id)),
            m500_mov_cn=sum(cn_yn),
            m500_mov_cn_h1=sum(h1), 
            m500_mov_cn_h2=sum(h2)) %>% as.data.frame %>% select(-geometry)
mov_yh2$m500_mov_total <- ifelse(mov_yh2$m500_mov_total, mov_yh2$m500_mov_total, NA)
write.csv(mov_yh2, '../03.Output/yh2_mov.csv', row.names=FALSE, na="")

# mov$yh == 3
mov_yh3 <- mov %>% filter(yh == 3) %>% select(-c(x, y, yh))
mov_yh3 <- st_join(doro_500m_buff_500m, mov_yh3, left=TRUE) %>% group_by(split_id) %>% 
  summarise(m500_mov_total=sum(!is.na(id)), 
            m500_mov_cn=sum(cn_yn), 
            m500_mov_cn_h1=sum(h1), 
            m500_mov_cn_h2=sum(h2)) %>% as.data.frame %>% select(-geometry)
mov_yh3$m500_mov_total <- ifelse(mov_yh3$m500_mov_total, mov_yh3$m500_mov_total, NA)
write.csv(mov_yh3, '../03.Output/yh3_mov.csv', row.names=FALSE, na="")



#####################################################
# 4.고정형 단속내역 (불법주정차) 관련 데이터셋 구축 #
#####################################################

# 데이터 불러오기
doro_500m_centro <- st_read("../03.Output/doro_500m_centro.shp", options="ENCODING=cp949") %>% st_transform(5179)
fix <- read.csv("../03.Output/fix.csv", encoding='cp949')
if (max(fix$y, na.rm=TRUE) > 39) {
  fix <- fix %>% st_as_sf(coords=c('x', 'y'), crs=5179, remove=FALSE)
} else {
  fix <- fix %>% st_as_sf(coords=c('x', 'y'), crs=4326, remove=FALSE) %>% st_transform(5179)
}

# 격자 생성
grd <- expand.grid(
  x = seq(from = min(st_coordinates(doro_500m_centro)[,1]), 
          to = max(st_coordinates(doro_500m_centro)[,1]), 
          by = 20), 
  y = seq(from = min(st_coordinates(doro_500m_centro)[,2]), 
          to = max(st_coordinates(doro_500m_centro)[,2]), 
          by = 20))
coordinates(grd) <- ~x+y
proj4string(grd) <- CRS("+init=epsg:5179")
gridded(grd) <- TRUE

# 고정형 집계 데이터 보간
idw_temp = gstat(formula=h1_cn_r ~ 1,
                 data=fix,
                 set=list(idp=2.0))
idw <- predict(object = idw_temp, newdata = grd) %>% raster()
doro_500m_centro$cn_r_cctv_fix_h1 = extract(idw, doro_500m_centro)

idw_temp = gstat(formula=h2_cn_r ~ 1,
                 data=fix,
                 set=list(idp=2.0))
idw <- predict(object = idw_temp, newdata = grd) %>% raster()
doro_500m_centro$cn_r_cctv_fix_h2 = extract(idw, doro_500m_centro)

doro_500m_centro %>% as.data.frame %>% select(split_id, cn_r_cctv_fix_h1, cn_r_cctv_fix_h2) %>% 
  write.csv("../03.Output/cn_r_fix.csv", row.names=FALSE)



####################################################
# 5. 고정형 단속내역 (도로방범) 관련 데이터셋 구축 #
####################################################

# 데이터 불러오기
doro_500m_centro <- st_read("../03.Output/doro_500m_centro.shp", options="ENCODING=cp949") %>% st_transform(5179)
road <- read.csv("../03.Output/road.csv", encoding='cp949')
if (max(road$y, na.rm=TRUE) > 39) {
  road <- road %>% st_as_sf(coords=c('x', 'y'), crs=5179, remove=FALSE)
} else {
  road <- road %>% st_as_sf(coords=c('x', 'y'), crs=4326, remove=FALSE) %>% st_transform(5179)
}

# 격자 생성
grd <- expand.grid(
  x = seq(from = min(st_coordinates(doro_500m_centro)[,1]), 
          to = max(st_coordinates(doro_500m_centro)[,1]), 
          by = 20), 
  y = seq(from = min(st_coordinates(doro_500m_centro)[,2]), 
          to = max(st_coordinates(doro_500m_centro)[,2]), 
          by = 20))
coordinates(grd) <- ~x+y
proj4string(grd) <- CRS("+init=epsg:5179")
gridded(grd) <- TRUE

# 고정형 집계 데이터 보간
idw_temp = gstat(formula= h1_cn_r ~ 1,
                 data=road,
                 nmax=length(road$id),
                 set=list(idp=2))
idw <- predict(object = idw_temp, newdata = grd) %>% raster()
doro_500m_centro$cn_r_cctv_road_h1 = extract(idw, doro_500m_centro)

idw_temp = gstat(formula= h2_cn_r ~ 1,
                 data=road,
                 nmax=length(road$id),
                 set=list(idp=2))
idw <- predict(object = idw_temp, newdata = grd) %>% raster()
doro_500m_centro$cn_r_cctv_road_h2 = extract(idw, doro_500m_centro)

doro_500m_centro %>% as.data.frame %>% select(split_id, cn_r_cctv_road_h1, cn_r_cctv_road_h2) %>% 
  write.csv("../03.Output/cn_r_road.csv", row.names=FALSE)



#######################################################
# 6. 직장주소 모델링 데이터셋 구축                    #
# 체납자 직장주소 데이터가 존재하지 않을 시 생략 가능 #
#######################################################

# 데이터 불러오기
work <- read.csv("../03.Output/work.csv", encoding='cp949')
doro_500m_buff_500m <- st_read("../03.Output/doro_500m_buff_500m.shp", options="ENCODING=cp949") %>% 
  st_transform(5179)
if (max(work$work_y, na.rm=TRUE) > 39) {
  work <- work %>% st_as_sf(coords=c('work_x', 'work_y'), crs=5179, remove=FALSE)
} else {
  work <- work %>% st_as_sf(coords=c('work_x', 'work_y'), crs=4326, remove=FALSE) %>% st_transform(5179)
}

# Count Points in Polygon
# work$yh1 == 1
work_yh1 <- work %>% filter(yh1 == 1)
doro_500m_buff_500m$m500_cn_work <- lengths(st_intersects(doro_500m_buff_500m, work_yh1))
doro_500m_buff_500m %>% as.data.frame %>% select(split_id, m500_cn_work) %>% 
  write.csv("../03.Output/yh1_work.csv", row.names=FALSE)

# work$yh2== 1
work_yh2 <- work %>% filter(yh2 == 1)
doro_500m_buff_500m$m500_cn_work <- lengths(st_intersects(doro_500m_buff_500m, work_yh2))
doro_500m_buff_500m %>% as.data.frame %>% select(split_id, m500_cn_work) %>% 
  write.csv("../03.Output/yh2_work.csv", row.names=FALSE)

# work$yh3 == 1
work_yh3 <- work %>% filter(yh3 == 1)
doro_500m_buff_500m$m500_cn_work <- lengths(st_intersects(doro_500m_buff_500m, work_yh3))
doro_500m_buff_500m %>% as.data.frame %>% select(split_id, m500_cn_work) %>% 
  write.csv("../03.Output/yh3_work.csv", row.names=FALSE)