library(data.table)
library(dplyr)
library(ggplot2)
library(forecast)
# library(knitr)

# setwd("D:/Dropbox/SNU_medical/corona_virus_est/mortality")
setwd("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/data")

# mortality_dat <- fread("korean_mortality_2010.csv", header = T, encoding = "UTF-8")
mortality_dat <- fread("korean_mortality_06.csv", header = T, encoding = "UTF-8")


# mort_dat_sel <- mortality_dat %>% filter(Local %in% c("Total", "Seoul", "Gyunggi", "Kyungbuk", "Kyungnam", "Daegu"))
mort_dat_sel <- reshape2::melt(mortality_dat, id =1, variable.name = "time", value.name = "mortal_num")

mort_dat_sel <- mort_dat_sel %>% mutate(year = substr(time, 1, 4) %>% as.character,
                                        month = substr(time, 7,8) %>% as.character,
                                        time_val = as.Date(paste0(year,"-", month, "-01"), format = "%Y-%m-%d")
) %>% dplyr::select(-time)

mort_dat_sel <- mort_dat_sel %>% mutate(mortal_num = mortal_num %>% as.numeric)


### Total mortality plot ------------

pdf("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/plot_final/totalmortal.pdf",
    width = 10, height = 7)
ggplot(mort_dat_sel, aes(y = mortal_num, x = time_val, color = Local)) + geom_line(size = 1) +
  scale_color_discrete(guide = guide_legend(reverse=TRUE)) + geom_vline(xintercept = "2020-01-01" %>% as.Date, linetype = 4) +
  xlim(c("2010-01-01" %>% as.Date, "2020-03-01" %>% as.Date)) + ylab("Mortality") + xlab("Year")
dev.off()
### Total Prediction -----------

ggplot(mort_dat_sel, aes(y = mortal_num, x = time_val, color = Local)) + geom_line(size = 1.1) +
  scale_color_discrete(guide = guide_legend(reverse=TRUE)) + geom_vline(xintercept = "2020-01-01" %>% as.Date, linetype = 4) +
  xlim(c("2010-01-01" %>% as.Date, "2020-03-01" %>% as.Date)) + ylab("Mortality") + xlab("Year")


ggplot(mort_dat_sel %>% filter(Local == "Total"), aes(y = mortal_num, x = time_val)) + geom_line()+
  geom_smooth() + ylab("Total Mortality") + xlab("Year") + ylim(c(0, 32000))


## Local ARIMA -----------

ARIMA_plot<- function(Local_n, date, xlim_date = "2019-01-01"){
  
  mort_dat_sel_tr <- mort_dat_sel %>% filter(time_val < date %>% as.Date)
  mort_dat_sel_te <- mort_dat_sel %>% filter(time_val >= date %>% as.Date)
  
  mort_dat_total <- mort_dat_sel_tr %>% filter(Local == Local_n)
  
  
  #fit1 <- arima(mort_dat_total$mortal_num, order = c(1, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
  #fit2 <- arima(mort_dat_total$mortal_num, order = c(2, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
  fit3 <- arima(mort_dat_total$mortal_num, order = c(2, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12))
  #fit4 <- arima(mort_dat_total$mortal_num, order = c(2, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))
  
  
  mort_dat_sel_te1 <- mort_dat_sel_te %>% filter(Local == Local_n)
  
  lag_m <- nrow(mort_dat_sel_te1)
  
  ypred <- forecast(fit3, lag_m)
  ## observation
  test1 <- data.frame(mort_dat_total, pred = ypred$fitted) %>% dplyr::select(time_val, mortal_num, pred)
  test2 <- data.frame(mort_dat_sel_te1 %>% dplyr::select(mortal_num, time_val), pred = ypred$mean, upper = ypred$upper,
                      lower = ypred$lower)
  
  tt <- test1 %>% full_join(test2, by = "time_val")
  tt <- tt %>% mutate(obs = case_when(mortal_num.x %>% is.na ~ mortal_num.y,
                                      TRUE ~ mortal_num.x),
                      pred = case_when(pred.x %>% is.na ~ pred.y,
                                       TRUE ~ pred.x)
  ) %>% dplyr::select(time_val, obs, pred, lower.95., upper.95.)
  tt <- tt %>% mutate(lower.95. = ifelse(lower.95. %>% is.na, pred, lower.95.),
                      upper.95. = ifelse(upper.95. %>% is.na, pred, upper.95.),)
  
  p1 <- ggplot(tt, aes(x = time_val, y = obs))+geom_line(aes(col = "black"), linetype = 1) + geom_line(aes(y=pred, col = "blue"), size = 1)+
    geom_ribbon(aes(ymin = lower.95., ymax = upper.95.), alpha=.2, fill = "blue")+xlim(c(xlim_date %>% as.Date, "2020-06-01" %>% as.Date))+
    scale_color_manual(values = c("black", "blue"), labels = c("Observed", "Predicted"))+
    theme(legend.title = element_blank(), legend.position = c(0.1, 0.9))+
    ylab("Mortality")+xlab("Date")+
    annotate("rect",xmin="2020-01-01" %>% as.Date ,xmax="2020-06-01" %>% as.Date,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")
  print(p1)
}

ARIMA_result<- function(Local_n, date){
  
  mort_dat_sel_tr <- mort_dat_sel %>% dplyr::filter(time_val < date %>% as.Date)
  mort_dat_sel_te <- mort_dat_sel %>% dplyr::filter(time_val >= date %>% as.Date)
  
  mort_dat_total <- mort_dat_sel_tr %>% dplyr::filter(Local == Local_n)
  
  
  #fit1 <- arima(mort_dat_total$mortal_num, order = c(1, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
  #fit2 <- arima(mort_dat_total$mortal_num, order = c(2, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
  fit3 <- arima(mort_dat_total$mortal_num, order = c(2, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12))
  #fit4 <- arima(mort_dat_total$mortal_num, order = c(2, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))
  
  
  mort_dat_sel_te1 <- mort_dat_sel_te %>% dplyr::filter(Local == Local_n)
  
  lag_m <- nrow(mort_dat_sel_te1)
  
  ypred <- forecast(fit3, lag_m)
  
  ## observation
  test1 <- data.frame(mort_dat_total, pred = ypred$fitted) %>% dplyr::select(time_val, mortal_num, pred)
  test2 <- data.frame(mort_dat_sel_te1 %>% dplyr::select(mortal_num, time_val), pred = ypred$mean, upper = ypred$upper,
                      lower = ypred$lower)
  
  tt <- test1 %>% full_join(test2, by = "time_val")
  tt <- tt %>% mutate(obs = case_when(mortal_num.x %>% is.na ~ mortal_num.y,
                                      TRUE ~ mortal_num.x),
                      pred = case_when(pred.x %>% is.na ~ pred.y,
                                       TRUE ~ pred.x)
  ) %>% dplyr::select(time_val, obs, pred, lower.95., upper.95.)
  
  result <- tt %>% filter(time_val >= date %>% as.Date)
  # result <- tt
  result <- result %>% mutate(excess = obs - pred,
                              excess_per = excess/pred*100)
  colnames(result) <- c("time_val", "obs", "pred", "lower", "upper", "excess", "excess_per")
  return(result)
}


## Local result --------
loc_n <- c("Total", "Seoul", "Busan", "Daegu","Incheon",
  "Gwangju", "Daejeon", "Ulsan", "Sejong", "Gyunggi",
  "Gwangwon", "Chungbuk", "Chungnam", "Jeonbuk", "Jeonnam", "Kyungbuk",
  "Kyungnam", "Jaeju")

result <- data.frame()
for(i in 1:length(loc_n)){
  test <- ARIMA_result(Local_n = loc_n[i], date = "2020-01-01")
  test$loc_n <- loc_n[i]
  result <- rbind(result, test)
}

fwrite(result, "ARIMA_result_0906.csv")


## Random Forest Interval --------------
# install.packages("remotes")
# remotes::install_github("haozhestat/rfinterval")
library(rfinterval)

source("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/rfinterval_jw.r", local = T)


## RF table ---------

RF_table<- function(Local_n, date, xlim_date = "2019-01-01", method){
  
  loc_dat <- mort_dat_sel %>% filter(Local == Local_n)
  loc_dat<- loc_dat %>% mutate(mortal_num_lag1 = mortal_num %>% lag(1),
                               mortal_num_lag2 = mortal_num %>% lag(2),
                               mortal_num_lag3 = mortal_num %>% lag(3),
                               mortal_num_lag4 = mortal_num %>% lag(4),
                               mortal_num_lag5 = mortal_num %>% lag(5),
                               mortal_num_slag1 = mortal_num %>% lag(12))
  
  loc_dat_tr <- loc_dat %>% filter(time_val < date %>% as.Date) %>% na.omit
  loc_dat_te <- loc_dat %>% filter(time_val >= date %>% as.Date)
  
  
  fit_rf <- rfinterval_jw(mortal_num ~ mortal_num_lag1 + mortal_num_lag2 + mortal_num_lag3+mortal_num_lag4+mortal_num_lag5+mortal_num_slag1,
                          train_data = loc_dat_tr, test_data = loc_dat_te, alpha = 0.05)
  
  
  lag_m <- length(fit_rf$testPred)
  ypred_tr <- predict(fit_rf$trainRF, loc_dat_tr)$prediction
  if(method == "oob"){
    ypred_te <- data.frame(fit_rf[[1]], mean = fit_rf$testPred)
  }else if(method == "sc"){
    ypred_te <- data.frame(fit_rf[[2]], mean = fit_rf$testPred)
  }else{
    ypred_te <- data.frame(fit_rf[[3]], mean = fit_rf$testPred)
  }
  
  
  ## observation
  # test1 <- data.frame(mort_dat_total, pred = ypred$fitted) %>% select(time_val, mortal_num, pred)
  test1 <- data.frame(loc_dat_tr, pred = ypred_tr) %>% dplyr::select(time_val, mortal_num, pred)
  test2 <- data.frame(loc_dat_te %>% dplyr::select(mortal_num, time_val), pred = ypred_te$mean, upper = ypred_te$upper,
                      lower = ypred_te$lower)
  
  tt <- test1 %>% full_join(test2, by = "time_val")
  tt <- tt %>% mutate(obs = case_when(mortal_num.x %>% is.na ~ mortal_num.y,
                                      TRUE ~ mortal_num.x),
                      pred = case_when(pred.x %>% is.na ~ pred.y,
                                       TRUE ~ pred.x)
  ) %>% dplyr::select(time_val, obs, pred, lower, upper)
  tt <- tt %>% mutate(lower = ifelse(lower %>% is.na, pred, lower),
                      upper = ifelse(upper %>% is.na, pred, upper))%>% 
    mutate(excess = obs - pred,
           excess_per = excess/pred*100) %>% filter(time_val >= date %>% as.Date)
  
return(tt)
}

loc_n <- c("Total", "Seoul", "Busan", "Daegu","Incheon",
           "Gwangju", "Daejeon", "Ulsan", "Sejong", "Gyunggi",
           "Gwangwon", "Chungbuk", "Chungnam", "Jeonbuk", "Jeonnam", "Kyungbuk",
           "Kyungnam", "Jaeju")

result_tab <- data.frame()
for(i in 1:length(loc_n)){
  table_oob <- RF_table(Local_n = loc_n[i], date = "2020-01-01", method = "oob")
  table_oob <- data.frame(table_oob, loc_n = loc_n[i], method = "oob")
  table_sc <- RF_table(Local_n = loc_n[i], date = "2020-01-01", method = "sc")
  table_sc <- data.frame(table_sc, loc_n = loc_n[i], method = "sc")
  table_qr <- RF_table(Local_n = loc_n[i], date = "2020-01-01", method = "quantreg")
  table_qr <- data.frame(table_qr, loc_n = loc_n[i], method = "qr")
  table_arima <- ARIMA_result(Local_n = loc_n[i], date = "2020-01-01")
  table_arima <- data.frame(table_arima, loc_n = loc_n[i], method = "SARIMA")
  
  
  result_sub <- rbind(table_oob, table_sc, table_qr, table_arima)
  result_tab <- rbind(result_tab, result_sub)
}


fwrite(result_tab, file = "C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/prediction_result.csv")


## RF plot ------ 


RF_plot<- function(Local_n, date, xlim_date = "2019-01-01", method){
  set.seed(2020)
  loc_dat <- mort_dat_sel %>% filter(Local == Local_n)
  loc_dat<- loc_dat %>% mutate(mortal_num_lag1 = mortal_num %>% lag(1),
                                   mortal_num_lag2 = mortal_num %>% lag(2),
                                   mortal_num_lag3 = mortal_num %>% lag(3),
                                   mortal_num_lag4 = mortal_num %>% lag(4),
                                   mortal_num_lag5 = mortal_num %>% lag(5),
                                   mortal_num_slag1 = mortal_num %>% lag(12))
  
  loc_dat_tr <- loc_dat %>% filter(time_val < date %>% as.Date) %>% na.omit
  loc_dat_te <- loc_dat %>% filter(time_val >= date %>% as.Date)
  
  
  fit_rf <- rfinterval_jw(mortal_num ~ mortal_num_lag1 + mortal_num_lag2 + mortal_num_lag3+mortal_num_lag4+mortal_num_lag5+mortal_num_slag1,
                       train_data = loc_dat_tr, test_data = loc_dat_te, alpha = 0.05)
  
  
  lag_m <- length(fit_rf$testPred)
  ypred_tr <- predict(fit_rf$trainRF, loc_dat_tr)$prediction
  if(method == "oob"){
    ypred_te <- data.frame(fit_rf[[1]], mean = fit_rf$testPred)
  }else if(method == "sc"){
    ypred_te <- data.frame(fit_rf[[2]], mean = fit_rf$testPred)
  }else{
    ypred_te <- data.frame(fit_rf[[3]], mean = fit_rf$testPred)
  }
  
  
  ## observation
  # test1 <- data.frame(mort_dat_total, pred = ypred$fitted) %>% select(time_val, mortal_num, pred)
  test1 <- data.frame(loc_dat_tr, pred = ypred_tr) %>% dplyr::select(time_val, mortal_num, pred)
  test2 <- data.frame(loc_dat_te %>% dplyr::select(mortal_num, time_val), pred = ypred_te$mean, upper = ypred_te$upper,
                      lower = ypred_te$lower)
  
  tt <- test1 %>% full_join(test2, by = "time_val")
  tt <- tt %>% mutate(obs = case_when(mortal_num.x %>% is.na ~ mortal_num.y,
                                      TRUE ~ mortal_num.x),
                      pred = case_when(pred.x %>% is.na ~ pred.y,
                                       TRUE ~ pred.x)
  ) %>% dplyr::select(time_val, obs, pred, lower, upper)
  tt <- tt %>% mutate(lower = ifelse(lower %>% is.na, pred, lower),
                      upper = ifelse(upper %>% is.na, pred, upper))
  
  p1 <- ggplot(tt, aes(x = time_val, y = obs))+geom_line(aes(col = "black"), linetype = 1) + geom_line(aes(y=pred, col = "blue"), size = 1)+
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=.2, fill = "blue")+xlim(c(xlim_date %>% as.Date, "2020-06-01" %>% as.Date))+
    scale_color_manual(values = c("black", "blue"), labels = c("Observed", "Predicted"))+
    theme(legend.title = element_blank(), legend.position = c(0.1, 0.9))+
    ylab("Mortality")+xlab("Date")+
    annotate("rect",xmin="2020-01-01" %>% as.Date ,xmax="2020-06-01" %>% as.Date,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")
  print(p1)
}

library(ggpubr)




p1<- ARIMA_plot(Local_n = "Daegu", date = "2020-01-01") + ggtitle("SARIMA")
p2<- RF_plot(Local_n = "Daegu", date = "2020-01-01", method = "oob") + ggtitle("OOB")
p3<- RF_plot(Local_n = "Daegu", date = "2020-01-01", method = "sc") + ggtitle("SC")
p4<- RF_plot(Local_n = "Daegu", date = "2020-01-01", method = "quantreg") + ggtitle("Quantile")

pp <- ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

pdf("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/plot_final/totalpred.pdf",
    height = 7, width = 10)
print(pp)
dev.off()


### all loc plot 0118 -------------

loc_n2 <- loc_n[-1]

for(i in 1:length(loc_n2)){
  p1<- ARIMA_plot(Local_n = loc_n2[i], date = "2020-01-01") + ggtitle("SARIMA")
  p2<- RF_plot(Local_n = loc_n2[i], date = "2020-01-01", method = "oob") + ggtitle("OOB")
  p3<- RF_plot(Local_n = loc_n2[i], date = "2020-01-01", method = "sc") + ggtitle("SC")
  p4<- RF_plot(Local_n = loc_n2[i], date = "2020-01-01", method = "quantreg") + ggtitle("Quantile")
  
  pp <- ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
  
  p_n <- paste0(loc_n2[i], "_pred.pdf")
  pdf(paste0("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/plot_final/supple/", p_n),
      height = 7, width = 10)
  print(pp)
  dev.off()
}
