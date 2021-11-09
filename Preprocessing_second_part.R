rm(list=objects())
library(mgcv)
library(forecast)
library(lubridate)
library(dplyr)
set.seed(42)
setwd("~/Desktop/Simulations Covid Article/data")

### - Charger et préparer les données
Data <- readRDS('Data_preprocessed.RDS')
Data$weights <- 1

# on omet les jours spéciaux 
BH <- unique(filter(Data, BH != 0)$DateD)
DayBeforeBH <- BH - duration(1, units = 'day')
DayAfterBH <- BH + duration(1, units = 'day')
Ponts = c(DayBeforeBH, BH, DayAfterBH)
sel <- which(Data$DateD %in% Ponts | Data$Christmas_break != 0 | Data$Summer_break != 0)
Data[sel, 'weights'] <- 0
date_fin_train <- ymd("2019-09-01")

regions <- levels(Data$region)
n_reg <- length(regions)

formule <- Load ~ s(as.numeric(Date), k=5) + WeekDays:DLS + s(toy, k=20, bs='cc') + s(as.numeric(Date), Temp, k=c(3,5)) +
  s(Temp_s95, k=10, bs = 'cs') + s(Temp_s99, k=10, bs = 'cs') + s(Temp_s99_min, Temp_s99_max) + Load.48:WeekDays + Load.336 

for (r in 1:(n_reg-1)){
  sel_r <- which(Data$region == regions[r] & Data$Date < date_fin_train & Data$weights > 0)
  residus <- rep(NA, length(sel_r))
  for (i in 0:47){
    sel_r_i <- which(Data$tod[sel_r] == i)
    g_reg <- gam(formule, data = Data[sel_r,][sel_r_i,])
    residus[sel_r_i] <- g_reg$residuals
    print(i)
  }
  g_reg_arima <- auto.arima(residus, max.p=100, max.q=100, ic='aic')
  residus_arima <- g_reg_arima$residuals
  quantiles <- quantile(residus_arima, probs = c(0.0001, 1-0.0001))
  sel_elim <- which(residus_arima <= quantiles[1] | residus_arima >= quantiles[2])
  sel_elim_tot <- sel_elim
  for (h in 1:95){
    sel_elim_tot <- c(sel_elim_tot, sel_elim + h)
  }
  Data$weights[sel_r][sel_elim_tot] <- 0
  print(paste0(round(100*length(sel_elim_tot)/length(sel_r), 2), " % des données éliminées en ", regions[r]))
  print(head(sel_elim))
}

meanLoad <- aggregate(filter(Data, Date < date_fin_train, weights >0)$Load,
                      by = list(filter(Data, Date < date_fin_train, weights >0)$region,
                                filter(Data, Date < date_fin_train, weights >0)$tod), FUN = mean, na.rm = T)
Data$meanLoad <- NA
for (r in 1:n_reg){
  for (i in 0:47){
    Data[Data$region == regions[r] & Data$tod == i, 'meanLoad'] <- 
      filter(meanLoad, Group.1 == regions[r] & Group.2 == i)$x
  }
}

saveRDS(Data, "Data_preprocessed_final.RDS")