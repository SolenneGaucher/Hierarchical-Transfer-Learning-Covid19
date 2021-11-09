rm(list=objects())
library(mgcv)
library(forecast)
library(lubridate)
library(dplyr)
setwd("~/Desktop/Simulations Covid Article/data")

### - Charger et préparer les données
Data <- readRDS("Data_preprocessed_final.RDS")
Data$Load_norm <- Data$Load/Data$meanLoad
Data$Load_norm.48 <- Data$Load.48/Data$meanLoad
Data$Load_norm.336 <- Data$Load.336/Data$meanLoad
Data$pred_norm <- NA
Data$residuals_norm <- NA
Data$residuals_norm.48 <- NA
Data$residuals_norm.336 <- NA
Data <- cbind(Data, data.frame(matrix(NA, nrow = dim(Data)[1], ncol = 9)))
date_fin_train <- ymd("2019-09-01")

regions <- levels(Data$region)
n_reg <- length(regions)

formule <- Load_norm ~ s(as.numeric(Date), k=5) + WeekDays:DLS + s(toy, k=20, bs='cc') + s(as.numeric(Date), Temp, k=c(3,5)) +
  s(Temp_s95, k=10, bs = 'cs') + s(Temp_s99, k=10, bs = 'cs') + s(Temp_s99_min, Temp_s99_max) + Load_norm.48:WeekDays + Load_norm.336 


## %%%%%%%%%%%%% Un modèle par demi-heure et par région %%%%%%%%%%%%%%% ##

for (r in 1:n_reg){
  for (i in 0:47){
    sel_r_train <- which(Data$region == regions[r] & Data$tod == i &
                           Data$Date < date_fin_train & Data$weights >0)
    sel_r <- which(Data$region == regions[r]& Data$tod == i)
    GAM <- bam(formule, data = Data[sel_r_train,])
    Data[sel_r, 'pred_norm'] <- predict(GAM, newdata = Data[sel_r,])
    Data[sel_r, 'residuals_norm'] <- Data[sel_r, 'Load_norm'] - Data[sel_r, 'pred_norm'] 
    Data[sel_r, 'residuals_norm.48'] <- c(Data[sel_r, 'residuals_norm'][1], Data[sel_r, 'residuals_norm'][1:(length(Data[sel_r, 'residuals_norm'])-1)])
    Data[sel_r, 'residuals_norm.336'] <- c(Data[sel_r, 'residuals_norm'][1:7], Data[sel_r, 'residuals_norm'][1:(length(Data[sel_r, 'residuals_norm'])-7)])
    Data[sel_r, (dim(Data)[2]-8):dim(Data)[2]] <- predict(GAM, newdata = Data[sel_r,], type = 'terms')
  }
  print(r)
}
names(Data)[(dim(Data)[2]-8):dim(Data)[2]] <- paste0("GAM_terms", 1:9)
Data <- Data[!is.na(Data$Load),]
saveRDS(Data, "Data_GAM.RDS")
