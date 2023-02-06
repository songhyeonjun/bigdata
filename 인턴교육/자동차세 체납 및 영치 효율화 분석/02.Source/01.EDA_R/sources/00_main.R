##
## 표준분석모델 자동차세 체납영치 
## 현황분석
##
setwd('C:/Users/user/Desktop/dongdaemoon/02.Source/01.EDA_R/')
# 패키지 설치(처음 실행시에만 수행)
source("./sources/x01_package_install.R", encoding = 'utf-8')

# 패키지 및 데이터 불러오기
source("./sources/01_load.R", encoding = 'utf-8')

# 데이터 기본 처리
source("./sources/02_pretreatment.R", encoding = 'utf-8')

# 시각화 함수 불러오기
source("./sources/03_plots.R", encoding = 'utf-8')


### 시각화 함수
# 시각화 함수는 plot1~plot14로 구성되어 있으며, 각각 하나의 그래프를 만들어 줍니다.
# 괄호 안에 옵션을 넣어 줄 수 있으며, (개별 설명 참조)
# 원하는 그래프의 'plot()'부분의 아무 곳에 커서를 두고 ctrl + enter키를 누르시면 됩니다.
plot1()  # 연도별 영치건수
         #   (데이터 범위가 연단위로 12월에 끝날 경우 괄호 안에 1 입력)
plot2(1)  # 월별 영치건수(기본: 마지막 해 제외)
         #   (데이터 범위가 연단위로서 12월 말까지일 경우 괄호 안에 1 입력하여 마지막 해도 집계)
plot3()  # 요일별 영치건수
plot4() # 체납액구별 체납건수
plot5()  # 최초납기별 평균체납액 및 체납건수
         #   (괄호 안에 그래프가 나타낼 가장 빠른 년도를 입력. 기본: 2015)
plot6()    # 차량연식별 체납자 수
plot7()  # 등록, 체납, 영치차량 개인/법인 구성비
plot8()  # 개인/법인별 체납자 비율(체납차/등록차)
plot9()  # 개인/법인별 평균 체납액
plot10()  # 등록, 체납, 영치차량 성별 구성비
plot11() # 성별 체납자 비율(체납차/등록차)
plot12() # 성별 평균 체납액

### 메타파일 내보내기 설정3
# R에서 보여지는 그래프를 Power point 등에서 사용하고자 할 때
# 메타파일(.emf파일)로 내보내기를 하시면 프로그램 내에서 편집이 가능합니다.
#  1. metaplot_on에 커서를 두고 ctrl + enter를 누름
#  2. 원하는 그래프의 plot__()부분에 커서를 두고 ctrl + enter
#  3. metaplot_off에 커서를 두고 ctrl + enter
# 1~3의 과정을 완료하시면 plot폴더에 생성됩니다.

metaplot_on("원하는 파일이름 입력") # 메타파일 출력 모드 켜기(괄호 내 쌍따옴표 안에 원하는 파일명 입력)
metaplot_off() # 메타파일 출력 모드 끄기
