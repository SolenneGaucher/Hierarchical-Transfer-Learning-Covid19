################################ One random forest per region #####################################

rm(list=objects())
library(lubridate)
library(dplyr)
library(glmnet)
library(ranger)
setwd("~/Desktop/Simulations Covid Article/data")

set.seed(42)

### - Load Data
Data <- readRDS("Data_GAM.RDS")
Data <- filter(Data, Date >= ymd("2019-09-01"), Date < ymd("2020-09-17"), weights > 0)

# Define usefull variables
lockdown_start <- ymd("2020-03-16")
regions <- levels(Data$region)
n_reg <- length(regions)

### We select 4, 9, 14 variables using Lasso for each region, and use themm in the formula with the variable "WeekDays"
GAM_terms <- paste0("GAM_terms", 1:9)[-c(4,5)] # we remove terms 4 et 5, corresponding to toy and date
google_terms <- names(Data)[23:28]
form <- as.formula(paste0("residuals_norm ~ WeekDays + residuals_norm.48 + residuals_norm.336 + GovernmentResponseTracker + ",
                                   paste0(GAM_terms, collapse = ' + '), " + ",
                                   paste0(google_terms, collapse = ' + ')))

# Initialise the results
forecast_date <- lockdown_start
results_GAM_RF <- data.frame(Date = Date(0), region = character(0), tod = numeric(0), Load_norm = numeric(0), meanLoad = numeric(0), 
                               GAM = numeric(0), residuals = numeric(0), mean = numeric(0),
                               q0.05 = numeric(0), q0.1 = numeric(0), q0.5 = numeric(0), q0.9 = numeric(0), q0.95 = numeric(0))
for (r in 1:n_reg){
  sel_r_test <- which(Data$region == regions[r] & Data$DateD <= lockdown_start)
  results_GAM_RF <- rbind(results_GAM_RF,
                            data.frame(Date = Data$Date[sel_r_test], region = Data$region[sel_r_test], tod = Data$tod[sel_r_test],
                                       Load_norm = Data$Load_norm[sel_r_test], meanLoad = Data$meanLoad[sel_r_test], 
                                       GAM = Data$pred_norm[sel_r_test], residual = Data$residuals_norm[sel_r_test],
                                       mean = 0, q0.05 = 0, q0.1 = 0, q0.5 = 0, q0.90 = 0, q0.95 = 0))
}
dates <- unique(filter(Data, region == regions[1], Date > lockdown_start + duration(1, units = "days"))$DateD)
N <- length(dates)

## %%%%%%%%%%%%% Compute the predictions for the pandemix period %%%%%%%%%%%%%%% ##
for (r in 1:n_reg){
  for (j in 1:N){
    forecast_date <- dates[j]
    # Train the RF using the avalaible pandemic data, produce forecasts
    for (i in 0:47){
      sel_r_test <- which(Data$region == regions[r]  & Data$tod == i & Data$DateD == forecast_date)
      if (length(sel_r_test) != 0){
        sel_r_train <- which(Data$region == regions[r] & Data$tod == i & Data$Date >= lockdown_start &  Data$Date < forecast_date)
        rf <- ranger(form, data = Data[sel_r_train,], quantreg = T)
        rf.quantiles <- predict(rf, type = "quantiles", data = Data[sel_r_test,], quantiles = c(0.05, 0.1, 0.5, 0.9, 0.95))$predictions
        results_GAM_RF <- rbind(results_GAM_RF,
                                  data.frame(Date = Data$Date[sel_r_test], region = Data$region[sel_r_test], tod = Data$tod[sel_r_test],
                                             Load_norm = Data$Load_norm[sel_r_test], meanLoad = Data$meanLoad[sel_r_test], 
                                             GAM = Data$pred_norm[sel_r_test], residual = Data$residuals_norm[sel_r_test],
                                             mean = predict(rf, data = Data[sel_r_test,])$predictions,
                                             q0.05 = rf.quantiles[1], q0.1 = rf.quantiles[2], q0.5 = rf.quantiles[3],
                                             q0.90 = rf.quantiles[4], q0.95 = rf.quantiles[5]))
        
      }
    }
  }
  print(regions[[r]])
}

# Save results
saveRDS(results_GAM_RF, 'results_individual_RF.RDS')

################################ Random forests common to all regions #####################################
rm(list=objects())
set.seed(42)

### - Load Data
Data <- readRDS("Data_GAM.RDS")
Data <- filter(Data, Date >= ymd("2019-09-01"), Date < ymd("2020-09-17"), weights > 0)

# Define usefull variables
lockdown_start <- ymd("2020-03-16")
regions <- levels(Data$region)
n_reg <- length(regions)

### formula for the rando forest
GAM_terms <- paste0("GAM_terms", 1:9)[-c(4,5)] # we remove terms 4 et 5, corresponding to toy and date
google_terms <- names(Data)[23:28]
form <- as.formula(paste0("residuals_norm ~ WeekDays + residuals_norm.48 + residuals_norm.336 + GovernmentResponseTracker + ",
                                   paste0(GAM_terms, collapse = ' + '), " + ",
                                   paste0(google_terms, collapse = ' + ')))


# Initialise the results
forecast_date <- lockdown_start
results_GAM_RF_com <- data.frame(Date = Date(0), region = character(0), tod = numeric(0), Load_norm = numeric(0), meanLoad = numeric(0), 
                                 GAM = numeric(0), residuals = numeric(0), mean = numeric(0),
                                 q0.05 = numeric(0), q0.1 = numeric(0), q0.5 = numeric(0), q0.9 = numeric(0), q0.95 = numeric(0))
for (r in 1:n_reg){
  sel_test <- which(Data$region == regions[r] & Data$DateD <= lockdown_start)
  results_GAM_RF_com <- rbind(results_GAM_RF_com,
                              data.frame(Date = Data$Date[sel_test], region = Data$region[sel_test], tod = Data$tod[sel_test],
                                         Load_norm = Data$Load_norm[sel_test], meanLoad = Data$meanLoad[sel_test], 
                                         GAM = Data$pred_norm[sel_test], residual = Data$residuals_norm[sel_test],
                                         mean = 0, q0.05 = 0, q0.1 = 0, q0.5 = 0, q0.90 = 0, q0.95 = 0))
}
dates <- unique(filter(Data, region == regions[1], Date > lockdown_start + duration(1, units = "days"))$DateD)
N <- length(dates)

## %%%%%%%%%%%%% Compute the predictions for the pandemix period %%%%%%%%%%%%%%% ##
for (j in 1:N){
  forecast_date <- dates[j]
  # Train the RF using the avalaible pandemic data, produce forecasts
  for (i in 0:47){
    sel_test <- which(Data$tod == i & Data$DateD == forecast_date)
    if (length(sel_test) != 0){
      sel_train <- which(Data$tod == i & Data$Date >= lockdown_start &  Data$Date < forecast_date)
      rf.com <- ranger(form, data = Data[sel_train,], quantreg = T)
      rf.com.quantiles <- predict(rf.com, type = "quantiles", data = Data[sel_test,], quantiles = c(0.05, 0.1, 0.5, 0.9, 0.95))$predictions
      results_GAM_RF_com <- rbind(results_GAM_RF_com,
                                  data.frame(Date = Data$Date[sel_test], region = Data$region[sel_test], tod = Data$tod[sel_test],
                                             Load_norm = Data$Load_norm[sel_test], meanLoad = Data$meanLoad[sel_test], 
                                             GAM = Data$pred_norm[sel_test], residual = Data$residuals_norm[sel_test],
                                             mean = predict(rf.com, data = Data[sel_test,])$predictions,
                                             q0.05 = rf.com.quantiles[,1], q0.1 = rf.com.quantiles[,2], q0.5 = rf.com.quantiles[,3],
                                             q0.90 = rf.com.quantiles[,4], q0.95 = rf.com.quantiles[,5]))
    }
  }
  if (j %%7 == 0){
    print(forecast_date)
  }
}

# Save results
saveRDS(results_GAM_RF_com, 'results_com_RF.RDS')