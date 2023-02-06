#### import lib & set-up ####
rm(list = ls())
set.seed(333);options(stringsAsFactors = F)

# import Lib.
pkg_list <-
  c('readr',
    'data.table',
    'dplyr',
    'mltools',
    'xgboost',
    'stringr',
    'Ckmeans.1d.dp',
    'caret',
    'ggplot2',
    'e1071')
lapply(pkg_list, require, character.only = TRUE)


#### FUNCTION ####

#  Null 값에 대한 0값 Fill
na_df <- function(df, x = 0) {
  df[is.na(df)] <- x
  return(df)
}


# XGBOOST Mdoel setting
xgboost_yc <- function(x, y, param = NULL) {
  if (is.null(param)) {
    param <- list(
      max_depth = 6,
      gamma = 1,
      eta = 0.1,
      lambda = 0.5,
      alpha = 0.1,
      scale_pos_weight = 0.5,
      subsample  = 0.7,
      eval_metric = 'error',
      max_delta_step = 1
    )
  }
  
  return(
    xgboost(
      params = param,
      data = as.matrix(x),
      label = y,
      objective = "binary:logistic",
      nrounds = 500
    )
  )
  
}

setwd('D:/dev/final_soc/script_v2/new_data/cw') #- set work directory

#### Import Data ####
#### doro.csv ####
doro_info <- read_csv("doro.csv") %>% na_df()

#### cn_r_fix ####
cctv_fix_check <- dir(pattern = 'cn_r_fix.csv') 
if (length(cctv_fix_check) > 0) {
  doro_info <- left_join(doro_info, read_csv(cctv_fix_check),
                         by = c('split_id')) %>%
                        arrange(split_id) %>% na_df
}

#### cn_r_road.csv ####
cn_r_road_check <- dir(pattern = 'cn_r_road.csv') 
if (length(cn_r_road_check) > 0) {
  doro_info <- left_join(doro_info, read_csv(cn_r_road_check),
                  by = c('split_id')) %>%
                  arrange(split_id) %>% na_df
}

#### yh*_cn.csv ####
yh_cn_check <- dir(pattern = 'cn.csv') %>% sort()

cn_ls <- lapply(yh_cn_check, read_csv) 

for(i in 1:length(cn_ls)) {
  cn_ls[[i]] <- left_join(doro_info, cn_ls[[i]])
  cn_ls[[i]]$yh <- i
}


#### yh*_mov.csv ####
yh_mov_check <- dir(pattern = 'mov.csv') %>% sort()

mov_ls <- lapply(yh_mov_check, read_csv) 

for(i in 1:length(mov_ls)) {
  mov_ls[[i]] <- left_join(doro_info, mov_ls[[i]])
  mov_ls[[i]]$yh <- i
}

cn_df <- full_join(bind_rows(cn_df),
                   bind_rows(mov_ls)) %>% na_df()


work_file_yn <- dir(pattern = 'work.csv') %>% sort()
if (length(work_file_yn) > 0) {
  work_df <-  lapply(work_file_yn, read_csv) 
  
  for (j in length(work_df)){
    work_df[[j]]$yh <- j  
  }
  
  cn_df <- left_join(cn_df, bind_rows(work_df),
                     by = c('split_id','yh')) %>% arrange(split_id) %>% na_df()
  
}

#### yc.csv ####
yc_df <- read_csv('yc.csv') 
yc_df$yc_bin <- 1


# Data Merge(join)
merge_df <- left_join(cn_df, yc_df, 
                      by = c('split_id','yh')) %>% na_df()

# memorise maximum yh Value
yh_cut <- max(merge_df$yh)

# memorise data for Output
cn_tmp <-  merge_df %>%
  select(yh, yc, yc_bin, everything()) %>%
  na_df()

# DATA Preprocessing # -> Log / Ratio  
cn_df <- cn_tmp %>% select(-RDS_MAN_NO,-center_x,-center_y) %>%
  mutate(
    ROAD_BT = log1p(ROAD_BT), # log 
    m500_man = ifelse(m500_cn_n != 0 ,  m500_man / m500_cn_n , 0), # ratio
    m500_sy = ifelse(m500_cn_n != 0 , m500_sy / m500_cn_n, 0), # ratio
    m500_sh = ifelse(m500_cn_n != 0 , m500_sh / m500_cn_n, 0), # ratio
    m500_ts = ifelse(m500_cn_n != 0 , m500_ts / m500_cn_n, 0), # ratio
    m500_hm = ifelse(m500_cn_n != 0 , m500_hm / m500_cn_n, 0), # ratio
    m500_cn_n = log1p(m500_cn_n), # log 
    m500_mov_cn = log1p(m500_mov_cn), # log 
    m500_mov_cn_h1 = log1p(m500_mov_cn_h1), # log 
    m500_mov_cn_h2 = log1p(m500_mov_cn_h2), # log 
    m500_mov_total = log1p(m500_mov_total), # log 
    m500_park_n = log1p(park_n), # log 
    m500_park_lot = log1p(park_lot) # log 
  ) %>%
  arrange(yh, split_id)

## Checking Column & Log-transformation

if (sum(str_detect(colnames(cn_tmp), 'work')) > 0) {
  cn_df <- cn_df %>% mutate(m500_cn_work = log1p(m500_cn_work) )
}

if (sum(str_detect(colnames(cn_tmp), 'cctv_fix')) > 0) {
  cn_df <- cn_df %>% mutate(
    cn_r_cctv_fix_h1 = log1p(cn_r_cctv_fix_h1),
    cn_r_cctv_fix_h2 = log1p(cn_r_cctv_fix_h2)
  )
}

if (sum(str_detect(colnames(cn_tmp), 'cctv_road')) > 0) {
  cn_df <- cn_df %>%
    mutate(
      cn_r_cctv_road_h1 = log1p(cn_r_cctv_road_h1),
      cn_r_cctv_road_h2 = log1p(cn_r_cctv_road_h2)
    )
}

# Data split  - h1 : morning / h2 : afternoon

dt_ls <-
  list(h1_df = cn_df[, !(str_detect(colnames(cn_df), 'h2'))] %>% arrange(split_id),
       h2_df =  cn_df[, !(str_detect(colnames(cn_df), 'h1'))] %>% arrange(split_id)) # remove column


# Make Train & Test Sets 

# Normal Version #### 
# df_orig <- list(
#   train_h1 = dt_ls$h1_df  %>% dplyr::filter(yh %in% c(yh_cut - 1,
#                                                       yh_cut - 2)) %>% select(-yh),
#   train_h2 = dt_ls$h2_df %>% dplyr::filter(yh %in% c(yh_cut - 1,
#                                                      yh_cut - 2)) %>% select(-yh),
#
#   test_h1 = dt_ls$h1_df  %>% dplyr::filter(yh == yh_cut) %>% select(-yh),
#   test_h2 = dt_ls$h2_df %>% dplyr::filter(yh == yh_cut) %>% select(-yh)
# )

# m500_cn_n remove Version ####
df_orig <- list(
  train_h1 = dt_ls$h1_df  %>%
    dplyr::filter(yh %in% c(yh_cut - 1, yh_cut - 2)) %>%
    mutate(yc_bin = ifelse(m500_cn_n == 0, 0, yc_bin)),
  
  train_h2 = dt_ls$h2_df %>%
    dplyr::filter(yh %in% c(yh_cut - 1, yh_cut - 2)) %>%
    mutate(yc_bin = ifelse(m500_cn_n == 0, 0, yc_bin)),
  
  test_h1 = dt_ls$h1_df  %>% dplyr::filter(yh == yh_cut) ,
  test_h2 = dt_ls$h2_df %>% dplyr::filter(yh == yh_cut)
)

#########  1 / Train Set ###########

train_ls <- list(

  y_h1 = df_orig$train_h1$yc_bin,
  y_h2 = df_orig$train_h2$yc_bin,
  
  x_h1 = df_orig$train_h1 %>% select(-split_id, -yc_bin, -yc, -yh),
  x_h2 = df_orig$train_h2 %>% select(-split_id, -yc_bin, -yc, -yh)
)

#########  1 / Test Set ###########

test_ls <- list(
  x_h1 = df_orig$test_h1 %>% select(-split_id, -yc_bin, -yc, -yh),
  x_h2 = df_orig$test_h2 %>% select(-split_id, -yc_bin, -yc, -yh)
)

#1. XGBOOST #
xg_md_h1 <- xgboost_yc(train_ls$x_h1,
                       train_ls$y_h1)

xg_md_h2 <- xgboost_yc(train_ls$x_h2,
                       train_ls$y_h2)

####### Importance Value ##########################################
imp_xg_h1 <- xgb.importance(model = xg_md_h1)
imp_xg_h2 <- xgb.importance(model = xg_md_h2)

imp_list  <- list(
  xg_h1 = data.frame(
    model_nm = 'xgboost_h1',
    value = imp_xg_h1$Feature,
    Gain = imp_xg_h1$Gain
  )  %>% arrange(-Gain),
  
  xg_h2 = data.frame(
    model_nm = 'xgboost_h2',
    value = imp_xg_h2$Feature,
    Gain = imp_xg_h2$Gain
  )  %>% arrange(-Gain)
)

# 1. predict XG
xg_pred_h1 <- predict(xg_md_h1,
                      as.matrix(test_ls$x_h1),
                      type = 'response')

xg_pred_h2 <- predict(xg_md_h2,
                      as.matrix(test_ls$x_h2),
                      type = 'response')

#### ORIG data set + Prediction ####
result_table <- bind_cols(
  cn_tmp %>% dplyr::filter(yh == yh_cut),
  data.frame(
    # XGBOoST
    xg.pred_h1 = xg_pred_h1,
    xg.pred_h2 = xg_pred_h2,
    xg.pred_yn_h1 = ifelse(xg_pred_h1 >= mean(xg_pred_h1), 1, 0),
    xg.pred_yn_h2 = ifelse(xg_pred_h2  >= mean(xg_pred_h2), 1, 0)
  )
)


############ REPORTING - Confusion Matrix & F1 Measure #########################
result_list <- list(
  conf_h1  = confusionMatrix(
    as.factor(result_table$xg.pred_yn_h1),
    as.factor(result_table$yc_bin),
    positive = '1'
  ),
  conf_h2 = confusionMatrix(
    as.factor(result_table$xg.pred_yn_h2),
    as.factor(result_table$yc_bin),
    positive = '1'
  ),
  f1_h1 = MLmetrics::F1_Score(
    as.factor(result_table$xg.pred_yn_h1),
    as.factor(result_table$yc_bin),
    positive = '1'
  ),
  f1_h2  = MLmetrics::F1_Score(
    as.factor(result_table$xg.pred_yn_h2),
    as.factor(result_table$yc_bin),
    positive = '1'
  )
)

result_md <- data.frame(
  model = c('xgboost_h1', 'xgboost_h2'),
  acc = c(result_list$conf_h1$overall[1],
          result_list$conf_h2$overall[1]) ,
  
  Sens = c(result_list$conf_h1$byClass[1],
           result_list$conf_h2$byClass[1]) ,
  
  spec = c(result_list$conf_h1$byClass[2],
           result_list$conf_h2$byClass[2]) ,
  
  B_acc = c(result_list$conf_h1$byClass[11],
            result_list$conf_h2$byClass[11]) ,
  
  F1_measure = c(result_list$f1_h1[1],
                 result_list$f1_h2[1])
)


#### Save a Result ####
# 1. Check & Make a Folder 
folder_nm <- 'result_folder'
if (!dir.exists(folder_nm)) {
  dir.create(folder_nm)
}

# 2. Save Importance Value / Plot 
# 2.1- csv File 
write.csv(
  imp_list %>% bind_rows(),
  paste0('./', folder_nm, '/importance_value.csv'),
  row.names = F
) 

# 2.1- png File ( h1 / h2 model )
ggsave(paste0('./', folder_nm, '/importance_h1.png'),
       plot = xgb.ggplot.importance(imp_xg_h1, n_clusters = 1)) 

ggsave(paste0('./', folder_nm, '/importance_h2.png'),
       plot = xgb.ggplot.importance(imp_xg_h2, n_clusters = 1))


# 3. save a Origin + Prediction Data
write.csv(result_table,
          paste0('./', folder_nm, '/result_predict.csv'),
          row.names = F)

# 4. save a Performance
write.csv(result_md,
          paste0('./', folder_nm, '/result_acc.csv'),
          row.names = F)
  

print("######## End ########")
