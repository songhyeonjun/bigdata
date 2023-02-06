##
## 현황분석 시각화 함수
##

plot_nm = "noname" # 메타파일 저장시 기본 파일명 설정

plot1 <- function(a = 0) {
    n_youngchi_by_year <- youngchi %>% filter(!is.na(영치일)) %>% group_by(format(영치일, "%Y")) %>% tally()
    colnames(n_youngchi_by_year) <- c("year", "youngchi")
  if (a) {
    ggplot(n_youngchi_by_year, aes(year, youngchi), axis) +
      geom_bar(stat="identity", width = 0.8, fill = c(rep("steelblue", nrow(n_youngchi_by_year)))) +
      geom_text(aes(label=youngchi), vjust=1.6, color="white", size=3.5) +
      labs(x="년도", y="영치건수") +
      theme_minimal() + 
      theme(axis.text=element_text(size=12))
  } else {
    ggplot(n_youngchi_by_year, aes(year, youngchi), axis) +
      geom_bar(stat="identity", width = 0.8, fill = c(rep("steelblue", nrow(n_youngchi_by_year) - 1), "darkgrey")) +
      geom_text(aes(label=youngchi), vjust=1.6, color="white", size=3.5) +
      labs(x="년도", y="영치건수") +
      theme_minimal() + 
      theme(axis.text=element_text(size=12))
  }
}

## 월별 영치건수(전년도 까지만)
plot2 <- function(a = 0) {
if (a) {
  n_youngchi_by_month <- youngchi %>% count(format(영치일, "%m"))
} else {
  n_youngchi_by_month <- youngchi %>% filter(영치일 < paste0(format(max(영치일, na.rm = TRUE), "%Y"),"-01-01")) %>% count(format(영치일, "%m"))
} 
  colnames(n_youngchi_by_month) <- c("month", "youngchi")
  
  ggplot(n_youngchi_by_month, aes(month, youngchi), axis) +
    geom_bar(stat="identity", fill = "aquamarine4", width = 0.8) +
    geom_text(aes(label=youngchi), vjust=-0.3, size=3.5) +
    labs(x="월", y="영치건수") +
    theme_minimal() +
    theme(axis.text=element_text(size=12))
}

## 요일별 영치건수(월-금)
plot3 <- function(a = 0) {
  n_youngchi_by_wd <- youngchi %>% count(format(youngchi$영치일, "%w"))
  colnames(n_youngchi_by_wd) <- c("weekday", "youngchi")

  ggplot(filter(n_youngchi_by_wd, weekday > 0 & weekday < 6), aes(weekday, youngchi), axis) +
    geom_bar(stat="identity", fill = "coral3", width = 0.8) +
    geom_text(aes(label=youngchi), vjust=-0.3, size=3.5) +
    labs(x="", y="") +
    scale_x_discrete(labels = c("월", "화", "수", "목", "금"))  +
    theme_minimal() +
    theme(axis.text=element_text(size=12))
}

# 체납액구별 체납건수
plot4 <- function() {
  n_chenap_by_price <- chenap %>% group_by(ceiling(체납액 / 100000)) %>% tally()
  colnames(n_chenap_by_price) <- c("price", "n")
  
  ggplot(n_chenap_by_price, aes(x = price * 10 - 5, y = n)) +
    geom_bar(stat="identity", fill = "steelblue3") +
    geom_text(aes(x= price * 10 - 5, y = n, label=n), vjust=-0.5, color="black", size=3) +
    labs(x="금액(만원)", y="") +
    scale_x_continuous(breaks = seq(0, max(n_chenap_by_price$price) * 10, 100)) +
    theme_minimal() +
    theme(axis.text=element_text(size=12, angle = 45))
}

ploplot4t <- function(a = "2015") {
  n_chenap_by_ym <- chenap %>% group_by(paste(format(chenap$영치일, "%Y"), get_halfyear(chenap$영치일))) %>% summarise(n = n(),price = mean(price, na.rm = TRUE))
  colnames(n_chenap_by_ym) <- c("ym", "chenap", "price")
  n_chenap_by_ym <- bind_rows(n_chenap_by_ym %>% filter(ym >= a),
                              n_chenap_by_ym %>% filter(ym < a) %>% summarise(ym = paste0(" ", a, "년 이전"), chenap = sum(chenap), price = mean(price, na.rm = TRUE)))
  
  ggplot(n_chenap_by_ym, aes(ym, chenap), axis) +
    geom_bar(stat="identity", fill = "steelblue", width = 0.8) +
    geom_text(aes(label=chenap), vjust=-0.3, size=3.5) +
    labs(x="", y="") +
    theme_minimal() +
    theme(axis.text=element_text(size=12))
}

plot5 <- function(a = "2015") {
  n_chenap_by_ym <- chenap %>% group_by(paste(format(chenap$영치일, "%Y"), get_halfyear(chenap$영치일))) %>% summarise(n = n(),price = mean(price, na.rm = TRUE))
  colnames(n_chenap_by_ym) <- c("ym", "chenap", "price")
  n_chenap_by_ym <- bind_rows(n_chenap_by_ym %>% filter(ym >= a),
                              n_chenap_by_ym %>% filter(ym < a) %>% summarise(ym = paste0(" ", a, "년 이전"), chenap = sum(chenap), price = mean(price, na.rm = TRUE)))
  ## 최초납기별 평균체납액 + 최초납기경과건수(line)
  sec_axis_rate <- max(n_chenap_by_ym$price, na.rm = TRUE) / max(n_chenap_by_ym$chenap, na.rm = TRUE)

  ggplot(n_chenap_by_ym, aes(ym, price), axis) +
    geom_bar(stat="identity", width = 0.8, fill = "steelblue") +
    geom_line(stat="identity", aes(ym, chenap * sec_axis_rate, group = 1), col = "red", size = 1) +
    geom_point(stat="identity", aes(ym, chenap * sec_axis_rate, group = 1), col = "red", size = 2) +
    scale_y_continuous(name = "평균체납액 ", 
                       breaks = seq(0, max(n_chenap_by_ym$price, na.rm = TRUE), 200000), 
                       labels = paste0(seq(0, max(n_chenap_by_ym$price, na.rm = TRUE), 200000)/10000,"만원"), 
                       sec.axis = sec_axis( ~ . / sec_axis_rate, name = "체납건수" )) +
    geom_text(aes(label=round(price)), vjust=1.5, size=3.5, col = "white") +
    geom_text(aes(ym, chenap * sec_axis_rate, label=chenap), vjust=-1, size=3.5, col = "grey") +
    labs(x="", y="") +
    theme_minimal() +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
}

plot6 <- function() {
  carreg_tmp = carreg %>% select(-car_age)
  chenap_in_carreg <- sqldf("select r.*, c.영치일 chenap_date, c.price price, c.car_age
                          from carreg_tmp r, chenap c
                          where r.car_num = c.car_num")
  n_chenap_by_carage <- chenap_in_carreg %>% filter(!is.na(car_age)) %>% group_by(car_age) %>% tally()
  n_carreg_by_carage <- carreg %>% filter(!is.na(car_age)) %>% group_by(car_age) %>% tally()
  chenap_rate_by_carage <- sqldf("
                            select c.car_age car_age, c.n chenap, r.n reg, (1.0 * c.n) / (1.0 * r.n) chenap_r
                            from n_chenap_by_carage c, n_carreg_by_carage r
                            where c.car_age = r.car_age")
  
  ggplot(chenap_rate_by_carage, aes(x = car_age, y = chenap_r * 100, fill = chenap), axis) +
    geom_bar(stat="identity", width = 0.5) +
    geom_text(aes(label=paste0(round(chenap_r * 100, 1),"%")), vjust=-0.5, size=2) +
    geom_hline(yintercept =  mean(chenap_rate * 100), lty="dashed", col = "red") +
    labs(x="", y="", fill = "체납건수") + 
    theme_minimal()
}

plot7 <- function() {
  ## 개인/법인별 체납/영치/등록
  n_chenap_by_chk <- chenap %>% filter(구분 %in% c("개인", "법인")) %>% count(chk)
  n_chenap_by_chk$comp_r <- n_chenap_by_chk$n / n_chenap
  if (nrow(n_chenap_by_chk) != 0) {
    n_chenap_by_chk$cat <- "chenap"    
  } else {
    print("Warning: 체납 데이터에 개인/법인 구분이 정확하지 않습니다.")
  }
  
  n_carreg_by_chk <- carreg %>% filter(차종 %in% c("개인", "법인"))  %>% count(chk)
  n_carreg_by_chk$comp_r <- n_carreg_by_chk$n / n_car
  if (nrow(n_carreg_by_chk) != 0) {
    n_carreg_by_chk$cat <- "carreg"    
  } else {
    print("Warning: 등록 데이터에 개인/법인 구분이 정확하지 않습니다.")
  }
  
  n_youngchi_by_chk <- youngchi %>% filter(chk %in% c("개인", "법인")) %>% count(chk)
  n_youngchi_by_chk$comp_r <- n_youngchi_by_chk$n / n_youngchi
  if (nrow(n_youngchi_by_chk) != 0) {
    n_youngchi_by_chk$cat <- "youngchi"    
  } else {
    print("Warning: 영치 데이터에 개인/법인 구분이 정확하지 않습니다.")
  }
  
  pdata <- ddply(rbind(n_chenap_by_chk, n_carreg_by_chk, n_youngchi_by_chk) %>% arrange(desc(chk)), .(cat),
                 transform, pos = cumsum(comp_r) - (0.5 * comp_r))

  ggplot() + geom_bar(aes(y = comp_r, x = cat, fill = chk), data = pdata, stat="identity") +
    geom_text(data=pdata, aes(x = cat, y = pos, label = paste0(round(comp_r * 100),"%")), size=4, col = "white") +
    labs(x="", y="", fill="구분") +
    scale_x_discrete(labels = c("등록차량", "체납차량", "영치차량"))  +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = c("darkseagreen", "plum3")) +
    theme(axis.text.y=element_text(size=14, face = "bold"))
}

plot8 <- function() {
  n_chenap_by_chk <- chenap %>% filter(구분 %in% c("개인", "법인")) %>% count(구분)
  n_chenap_by_chk$comp_r <- n_chenap_by_chk$n / n_chenap
  n_chenap_by_chk$cat <- "chenap"
  
  n_carreg_by_chk <- carreg %>% filter(구분 %in% c("개인", "법인"))  %>% count(구분)
  n_carreg_by_chk$comp_r <- n_carreg_by_chk$n / n_car
  n_carreg_by_chk$cat <- "ccarreg"
  
  ## 개인/법인별 체납율 구하기
  chenap_rate_by_chk <- sqldf("
                            select c.chk chk, c.n chenap, r.n reg, (1.0 * c.n) / (1.0 * r.n) chenap_r
                            from n_chenap_by_chk c, n_carreg_by_chk r
                            where c.chk = r.chk")
  
  ggplot(chenap_rate_by_chk, aes(chk, chenap_r * 100, fill = chk), axis) +
    geom_bar(stat="identity", width = 0.5) +
    geom_text(aes(label=paste0(round(chenap_r * 100, 1),"%")), vjust=1.6, color="white", size=4) +
    labs(x="", y="") +
    scale_fill_manual(values = c("darkseagreen", "plum3")) +
    theme_minimal() + 
    theme(legend.position = "none") +
    theme(axis.text=element_text(size=12))
}

plot9 <- function() {
  ## 개인/법인별 체납액 구하기
  chenap_price_by_chk <- chenap %>% filter(!is.na(구분)) %>% group_by(구분) %>% summarise(체납액 = mean(체납액, na.rm = TRUE))
  mp <- max(chenap_price_by_chk$체납액)
  
  ggplot(chenap_price_by_chk, aes(구분, 체납액, fill = 구분), axis) +
    geom_bar(stat="identity", width = 0.5) +
    geom_text(aes(label=round(체납액)), vjust=1.6, color="white", size=4) +
    labs(x="", y="") +
    scale_fill_manual(values = c("darkseagreen", "plum3")) +
    scale_y_continuous(breaks = seq(0, mp, 100000), labels = paste0(seq(0, floor(mp/100000), 1),"0 만원")) +
    theme_minimal() + 
    theme(legend.position = "none") +
    theme(axis.text=element_text(size=12))
}

plot10 <- function() {
  ## 성별 체납/영치/등록
  n_chenap_by_sex <- chenap %>% filter(sex %in% c("남자", "여자")) %>% group_by(sex) %>% tally()
  colnames(n_chenap_by_sex) <- c("sex", "n")
  n_chenap_by_sex$comp_r <- n_chenap_by_sex$n / sum(n_chenap_by_sex$n)
  if(nrow(n_chenap_by_sex) != 0) {
    n_chenap_by_sex$cat <- "chenap"
  } else {
    print("Warning: 체납 데이터에 성별이 정확하지 않습니다.")
  }
  
  n_carreg_by_sex <- carreg %>% filter(sex %in% c("남자", "여자")) %>% group_by(sex) %>% tally()
  colnames(n_carreg_by_sex) <- c("sex", "n")
  n_carreg_by_sex$comp_r <- n_carreg_by_sex$n / sum(n_carreg_by_sex$n)
  if(nrow(n_carreg_by_sex) != 0) {
    n_carreg_by_sex$cat <- "carreg"
  } else {
    print("Warning: 차량등록 데이터에 성별이 정확하지 않습니다.")
  }
  
  n_youngchi_by_sex <- youngchi %>% filter(sex %in% c("남자", "여자")) %>% group_by(sex) %>% tally()
  colnames(n_youngchi_by_sex) <- c("sex", "n")
  n_youngchi_by_sex$comp_r <- n_youngchi_by_sex$n / sum(n_youngchi_by_sex$n)
  if(nrow(n_youngchi_by_sex) != 0) {
    n_youngchi_by_sex$cat <- "youngchi"
  } else {
    print("Warning: 영치 데이터에 성별이 정확하지 않습니다.")
  }
  
  pdata <- ddply(rbind(n_chenap_by_sex, n_carreg_by_sex, n_youngchi_by_sex) %>% arrange(sex), .(cat),
                 transform, pos = cumsum(comp_r) - (0.5 * comp_r))
  
  ggplot() + geom_bar(aes(y = comp_r, x = cat, fill = sex), data = pdata, stat="identity", position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = c("steelblue1", "indianred1")) +
    geom_text(data=pdata, aes(x = cat, y = pos, label = paste0(round(comp_r * 100, 1),"%")), size=4, col = "white") +
    labs(x="", y="", fill="구분") +
    scale_x_discrete(labels = c("등록차량", "체납차량", "영치차량"))  +
    coord_flip() +
    theme_minimal() +
    theme(axis.text=element_text(size=12))
}

## 성별 체납율 구하기
plot11 <- function() {
  n_chenap_by_sex <- youngchi %>% filter(성별 %in% c("남", "여")) %>% group_by(성별) %>% tally()
  colnames(n_chenap_by_sex) <- c("성별", "n")
  n_chenap_by_sex$comp_r <- n_chenap_by_sex$n / sum(n_chenap_by_sex$n)
  n_chenap_by_sex$cat <- "youngchi"
  
  n_carreg_by_sex <- youngchi %>% filter(성별 %in% c("남", "여")) %>% group_by(성별) %>% tally()
  colnames(n_carreg_by_sex) <- c("성별", "n")
  n_carreg_by_sex$comp_r <- n_carreg_by_sex$n / sum(n_carreg_by_sex$n)
  n_carreg_by_sex$cat <- "youngchi"
  
  chenap_rate_by_sex <- sqldf("select c.성별 sex, c.n chenap, r.n reg, (1.0 * c.n) / (1.0 * r.n) chenap_r
                            from n_chenap_by_sex c, n_carreg_by_sex r
                            where c.성별 = r.성별") %>% filter(sex %in% c("남", "여"))

  ggplot(chenap_rate_by_sex, aes(x = sex, y = chenap_r * 100, fill = sex), axis) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values = c("steelblue1", "indianred1")) +
    geom_text(aes(label=paste0(round(chenap_r * 100, 1),"%")), vjust=1.6, color="white", size=3.5) +
    labs(x="", y="", fill = "sex") + 
    theme_minimal() + 
    theme(legend.position = "none")
}

## 성별 체납액 구하기
plot12 <- function() {
  chenap_price_by_sex <- youngchi %>% filter(성별 %in% c("남", "여")) %>% group_by(성별) %>% summarise(체납금액 = mean(체납금액, na.rm = TRUE))
  mp <- max(chenap_price_by_sex$체납금액)
    
  ggplot(chenap_price_by_sex, aes(x = 성별, y = 체납금액, fill = 성별), axis) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values = c("steelblue1", "indianred1")) +
    scale_y_continuous(breaks = seq(0, mp + 100000, 100000), labels = paste0(seq(0, floor(mp/10000) + 10, 10), "만원")) +
    geom_text(aes(label=round(체납금액)), vjust=1.6, color="white", size=3.5) +
    labs(x="", y="", fill = "성별") + 
    theme_minimal() + 
    theme(legend.position = "none")
}

## 메타파일 저장용
# 메타파일 연결 열기
metaplot_on <- function(nm = format(Sys.time(), "%Y%m%d-%H%M%S")) {
  win.metafile(paste0("./plot/",nm,".emf"),width=17, height=9)
  print(paste0("그래프가 ", nm, ".emf 메타파일로 plot 폴더에 저장됩니다."))
}

#메타파일 연결 닫기
# 모든 메타파일과의 연결을 닫아 주지만 on - off를 반복 1회씩 사용할 것 권장
metaplot_off <- function() {
  meta_x <- dev.list()[grep('^win.metafile', names(dev.list()))]
  for (x in meta_x) {
    dev.off(x)
  }
  print("메타파일과의 연결을 닫습니다. 그래프가 오른쪽 plot창에 출력됩니다.")
}

print("03_plots: 시각화 함수 로드 완료")