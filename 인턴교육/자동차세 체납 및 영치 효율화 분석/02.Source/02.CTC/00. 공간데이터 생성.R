rm(list = ls())
set.seed(333);options(stringsAsFactors = F)

# install.packages("readxl")

# import Lib.
pkg_list <-
  c('sf',
    'raster',
    'dplyr',
    'readxl',
    'gstat',
    'data.table',
    'lwgeom')

# install.packages(pkg_list)

lapply(pkg_list, require, character.only = TRUE)

setwd('C:/Users/user/Desktop/dongdaemoon/01.Data/')

# 2021.01.05
######################################################################################
                                   # 공간 데이터 생성 #
######################################################################################

# 데이터 불러오기

youngchi <- read.csv('영치차량내역서(수정본).csv', encoding='cp949', fileEncoding = 'euc-kr')
youngchi2 <- read.csv('체납자주소좌표.csv', encoding='cp949', fileEncoding = 'euc-kr')
chenap <- read.csv('자동차세 체납자료(동대문구만)_좌표추가.csv', encoding='cp949', fileEncoding = 'euc-kr')
carreg <- read.csv('차량등록현황_좌표추가.csv', encoding='cp949', fileEncoding = 'euc-kr')

# 01_youngchi_loc.shp 생성
if (max(youngchi$lat, na.rm=TRUE) > 39) {
  youngchi %>% filter(!is.na(lng) | !is.na(lat)) %>% st_as_sf(coords=c('lng', 'lat'), crs=5179, remove=FALSE) %>% st_write("../03.Output/01_youngchi_loc.shp", layer_options="ENCODING=cp949")
} else {
  youngchi %>% filter(!is.na(lng) | !is.na(lat)) %>% st_as_sf(coords=c('lng', 'lat'), crs=4326, remove=FALSE) %>% st_transform(5179) %>% st_write("../03.Output/01_youngchi_loc.shp", layer_options="ENCODING=cp949")
}

# 01_youngchi_adr.shp 생성
if (max(youngchi2$Y, na.rm=TRUE) > 39) {
  youngchi2 %>% filter(!is.na(X) | !is.na(Y)) %>% st_as_sf(coords=c('X', 'Y'), crs=5179, remove=FALSE) %>% st_write("../03.Output/01_youngchi_adr.shp", layer_options="ENCODING=cp949")
} else {
  youngchi2 %>% filter(!is.na(X) | !is.na(Y)) %>% st_as_sf(coords=c('X', 'Y'), crs=4326, remove=FALSE) %>% st_transform(5179) %>% st_write("../03.Output/01_youngchi_adr.shp", layer_options="ENCODING=cp949")
}

# 02_chenap_adr.shp 생성
if (max(chenap$Y, na.rm = TRUE) > 39) {
  chenap %>% filter(!is.na(X) | !is.na(Y)) %>% st_as_sf(coords=c('X', 'Y'), crs=5179, remove=FALSE) %>% st_write("../03.Output/02_chenap_adr.shp", layer_options="ENCODING=cp949")
} else {
  chenap %>% filter(!is.na(X) | !is.na(Y)) %>% st_as_sf(coords=c('X', 'Y'), crs=4326, remove=FALSE) %>% st_transform(5179) %>% st_write("../03.Output/02_chenap_adr.shp", layer_options="ENCODING=cp949")
}

# 02_chenap_work.shp 생성
# 체납자 직장주소 데이터가 존재하지 않을 시 생략 가능
if (max(chenap$work_y, na.rm = TRUE) > 39) {
  chenap %>% filter(!is.na(work_x) | !is.na(work_y)) %>% st_as_sf(coords=c('work_x', 'work_y'), crs=5179, remove=FALSE) %>% st_write("../03.Output/02_chenap_work.shp", layer_options="ENCODING=cp949")
} else {
  chenap %>% filter(!is.na(work_x) | !is.na(work_y)) %>% st_as_sf(coords=c('work_x', 'work_y'), crs=4326, remove=FALSE) %>% st_transform(5179) %>% st_write("../03.Output/02_chenap_work.shp", layer_options="ENCODING=cp949")
}

# 03_carreg_adr.shp 생성
if (max(carreg$Y, na.rm = TRUE) > 39) {
  carreg %>% filter(!is.na(X) | !is.na(Y)) %>% st_as_sf(coords=c('X', 'Y'), crs=5179, remove=FALSE) %>% st_write("../03.Output/03_carreg_adr.shp", layer_options="ENCODING=cp949")
} else {
  carreg %>% filter(!is.na(X) | !is.na(Y)) %>% st_as_sf(coords=c('X', 'Y'), crs=4326, remove=FALSE) %>% st_transform(5179) %>% st_write("../03.Output/03_carreg_adr.shp", layer_options="ENCODING=cp949")
}
