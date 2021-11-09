rm(list=objects())
library(lubridate)
library(dplyr)
library(glmnet)
library(ranger)
library(Boruta)
library(VSURF)
setwd("~/Desktop/Simulations Covid Article/data")

set.seed(42)

### - Load Data
Data <- readRDS("Data_GAM.RDS")
Data <- filter(Data, Date >= ymd("2019-09-01"), Date < ymd("2020-09-17"), weights > 0)
Data <- filter(Data, region == 'France')

# Define usefull variables
lockdown_start <- ymd("2020-03-16")

### We select 4, 9, 14 variables using Lasso for each region, and use themm in the formula with the variable "WeekDays"
GAM_terms <- paste0("GAM_terms", 1:9)[-c(4,5)] # we remove terms 4 et 5, corresponding to toy and date
google_terms <- names(Data)[23:28]

complete_formula <- as.formula(paste0("residuals_norm ~ residuals_norm.48 + residuals_norm.336 + GovernmentResponseTracker + ",
                                      paste0(GAM_terms, collapse = ' + '), " + ",
                                      paste0(google_terms, collapse = ' + ')))
variables <- all.vars(complete_formula)[-1]
n_var <- length(variables)
formulas <- list() # formulas[[x]]stores the formula with c(5,10,15)[x] variables for regions[r]

# Initialise the formulas used for the RF using the two weeks before the start of the lockdown
sel <- which(Data$Date < lockdown_start &  Data$Date >= ymd("2020-03-01"))
lasso <- glmnet(x = as.matrix(Data[sel, variables]), y = Data[sel, 'residuals_norm'], alpha = 1)
if (max(lasso$df) >= 4){
  lambda.5 <- lasso$lambda[min(which((lasso$df >= 4)))]
}else{
  lambda.5 <- lasso$lambda[length(lasso$df)]
}
vars.5 <- variables[which(glmnet(x = as.matrix(Data[sel, variables]), y = Data[sel, 'residuals_norm'], alpha = 1, lambda = lambda.5)$beta != 0)]
formulas[[1]] <- paste0("residuals_norm ~ WeekDays + ", paste0(vars.5, collapse = ' + '))
if (max(lasso$df) >= 9){
  lambda.10 <- lasso$lambda[min(which((lasso$df >= 9)))]
}else{
  lambda.10 <- lasso$lambda[length(lasso$df)]
}
vars.10 <- variables[which(glmnet(x = as.matrix(Data[sel, variables]), y = Data[sel, 'residuals_norm'], alpha = 1, lambda = lambda.10)$beta != 0)]
formulas[[2]] <- paste0("residuals_norm ~ WeekDays + ", paste0(vars.10, collapse = ' + '))
formulas[[3]] <- as.formula(paste0("residuals_norm ~ WeekDays + residuals_norm.48 + residuals_norm.336 + GovernmentResponseTracker + ",
                                   paste0(GAM_terms, collapse = ' + '), " + ",
                                   paste0(google_terms, collapse = ' + ')))
formulas[[4]] <- paste0("residuals_norm ~ ", paste0(getSelectedAttributes(Boruta(formulas[[3]], data=Data[sel,])), collapse = ' + '))



print(formulas)

# Initialise the results
forecast_date <- lockdown_start
results_GAM_RF_5 <- data.frame(Date = Date(0), region = character(0), tod = numeric(0), Load_norm = numeric(0), meanLoad = numeric(0), 
                               GAM = numeric(0), residuals = numeric(0), q0.5 = numeric(0))
results_GAM_RF_10 <- data.frame(Date = Date(0), region = character(0), tod = numeric(0), Load_norm = numeric(0), meanLoad = numeric(0), 
                                GAM = numeric(0), residuals = numeric(0),q0.5 = numeric(0))
results_GAM_RF_tot <- data.frame(Date = Date(0), region = character(0), tod = numeric(0), Load_norm = numeric(0), meanLoad = numeric(0), 
                                 GAM = numeric(0), residuals = numeric(0),q0.5 = numeric(0))
results_GAM_RF_Boruta <- data.frame(Date = Date(0), region = character(0), tod = numeric(0), Load_norm = numeric(0), meanLoad = numeric(0), 
                                    GAM = numeric(0), residuals = numeric(0), q0.5 = numeric(0))

sel_test <- which(Data$DateD == lockdown_start)
results_GAM_RF_5 <- rbind(results_GAM_RF_5,
                          data.frame(Date = Data$Date[sel_test], region = Data$region[sel_test], tod = Data$tod[sel_test],
                                     Load_norm = Data$Load_norm[sel_test], meanLoad = Data$meanLoad[sel_test], 
                                     GAM = Data$pred_norm[sel_test], residual = Data$residuals_norm[sel_test],
                                     q0.5 = 0))
results_GAM_RF_10 <- rbind(results_GAM_RF_10,
                           data.frame(Date = Data$Date[sel_test], region = Data$region[sel_test], tod = Data$tod[sel_test],
                                      Load_norm = Data$Load_norm[sel_test], meanLoad = Data$meanLoad[sel_test], 
                                      GAM = Data$pred_norm[sel_test], residual = Data$residuals_norm[sel_test],
                                      q0.5 = 0))
results_GAM_RF_tot <- rbind(results_GAM_RF_tot,
                            data.frame(Date = Data$Date[sel_test], region = Data$region[sel_test], tod = Data$tod[sel_test],
                                       Load_norm = Data$Load_norm[sel_test], meanLoad = Data$meanLoad[sel_test], 
                                       GAM = Data$pred_norm[sel_test], residual = Data$residuals_norm[sel_test],
                                       q0.5 = 0))
results_GAM_RF_Boruta <- rbind(results_GAM_RF_Boruta,
                               data.frame(Date = Data$Date[sel_test], region = Data$region[sel_test], tod = Data$tod[sel_test],
                                          Load_norm = Data$Load_norm[sel_test], meanLoad = Data$meanLoad[sel_test], 
                                          GAM = Data$pred_norm[sel_test], residual = Data$residuals_norm[sel_test],
                                          q0.5 = 0))

dates <- unique(filter(Data, Date > lockdown_start + duration(1, units = "days"))$DateD)
N <- length(dates)

## %%%%%%%%%%%%% Compute the predictions for the pandemix period %%%%%%%%%%%%%%% ##
for (j in 1:N){
  forecast_date <- dates[j]
  if (j %% 7 == 0){ # update formulas every week
    sel <- which(Data$Date >= lockdown_start &  Data$Date < forecast_date)
    lasso <- glmnet(x = as.matrix(Data[sel, variables]), y = Data[sel, 'residuals_norm'], alpha = 1)
    if (max(lasso$df) >= 4){
      lambda.5 <- lasso$lambda[min(which((lasso$df >= 4)))]
    }else{
      lambda.5 <- lasso$lambda[length(lasso$df)]
    }
    vars.5 <- variables[which(glmnet(x = as.matrix(Data[sel, variables]), y = Data[sel, 'residuals_norm'], alpha = 1, lambda = lambda.5)$beta != 0)]
    formulas[[1]]<- paste0("residuals_norm ~ WeekDays + ", paste0(vars.5, collapse = ' + '))
    if (max(lasso$df) >= 9){
      lambda.10 <- lasso$lambda[min(which((lasso$df >= 9)))]
    }else{
      lambda.10 <- lasso$lambda[length(lasso$df)]
    }
    vars.10 <- variables[which(glmnet(x = as.matrix(Data[sel, variables]), y = Data[sel, 'residuals_norm'], alpha = 1, lambda = lambda.10)$beta != 0)]
    formulas[[2]]<- paste0("residuals_norm ~ WeekDays + ", paste0(vars.10, collapse = ' + '))
    
    vars.Boruta <- getSelectedAttributes(Boruta(formulas[[3]], data=Data[sel,]))
    formulas[[4]]<- as.formula(paste0("residuals_norm ~ ", paste0(vars.Boruta, collapse = ' + ')))
    print(forecast_date)
    print(formulas[[4]])
  }
  # Train the RF using the avalaible pandemic data, produce forecasts
  for (i in 0:47){
    sel_test <- which(Data$tod == i & Data$DateD == forecast_date)
    if (length(sel_test) != 0){
      sel_train <- which(Data$tod == i & Data$Date >= lockdown_start &  Data$Date < forecast_date)
      rf.5 <- ranger(formulas[[1]], data = Data[sel_train,], quantreg = T)
      rf.5.quantiles <- predict(rf.5, type = "quantiles", data = Data[sel_test,], quantiles = c(0.5, 0.1, 0.5, 0.9, 0.95))$predictions
      results_GAM_RF_5 <- rbind(results_GAM_RF_5,
                                data.frame(Date = Data$Date[sel_test], region = Data$region[sel_test], tod = Data$tod[sel_test],
                                           Load_norm = Data$Load_norm[sel_test], meanLoad = Data$meanLoad[sel_test], 
                                           GAM = Data$pred_norm[sel_test], residual = Data$residuals_norm[sel_test],
                                           q0.5 = rf.5.quantiles[1]))
      
      rf.10 <- ranger(formulas[[2]], data = Data[sel_train,], quantreg = T)
      rf.10.quantiles <- predict(rf.10, type = "quantiles", data = Data[sel_test,], quantiles = c(0.5))$predictions
      results_GAM_RF_10 <- rbind(results_GAM_RF_10,
                                 data.frame(Date = Data$Date[sel_test], region = Data$region[sel_test], tod = Data$tod[sel_test],
                                            Load_norm = Data$Load_norm[sel_test], meanLoad = Data$meanLoad[sel_test], 
                                            GAM = Data$pred_norm[sel_test], residual = Data$residuals_norm[sel_test],
                                            q0.5 = rf.10.quantiles[1]))
      rf.tot <- ranger(formulas[[3]], data = Data[sel_train,], quantreg = T)
      rf.tot.quantiles <- predict(rf.tot, type = "quantiles", data = Data[sel_test,], quantiles = c(0.5, 0.1, 0.5, 0.9, 0.95))$predictions
      results_GAM_RF_tot <- rbind(results_GAM_RF_tot,
                                  data.frame(Date = Data$Date[sel_test], region = Data$region[sel_test], tod = Data$tod[sel_test],
                                             Load_norm = Data$Load_norm[sel_test], meanLoad = Data$meanLoad[sel_test], 
                                             GAM = Data$pred_norm[sel_test], residual = Data$residuals_norm[sel_test],
                                             q0.5 = rf.tot.quantiles[1]))
      rf.Boruta <- ranger(formulas[[4]], data = Data[sel_train,], quantreg = T)
      rf.Boruta.quantiles <- predict(rf.Boruta, type = "quantiles", data = Data[sel_test,], quantiles = c(0.5, 0.1, 0.5, 0.9, 0.95))$predictions
      results_GAM_RF_Boruta <- rbind(results_GAM_RF_Boruta,
                                     data.frame(Date = Data$Date[sel_test], region = Data$region[sel_test], tod = Data$tod[sel_test],
                                                Load_norm = Data$Load_norm[sel_test], meanLoad = Data$meanLoad[sel_test], 
                                                GAM = Data$pred_norm[sel_test], residual = Data$residuals_norm[sel_test],
                                                q0.5 = rf.Boruta.quantiles[1]))
    }
  }
}

# Save results
saveRDS(results_GAM_RF_5, 'results_VarSel_5.RDS')
saveRDS(results_GAM_RF_10, 'results_VarSel_10.RDS')
saveRDS(results_GAM_RF_tot, 'results_VarSel_tot.RDS')
saveRDS(results_GAM_RF_Boruta, 'results_VarSel_Boruta.RDS')


## %%%%%%%%%%%%% Comare variable selection methods %%%%%%%%%%%%%%% ##


rm(list=objects())
library(lubridate)
library(dplyr)
library(opera)
setwd("~/Desktop/Simulations Covid Article/data")

# Agregate results of the individual quantile RF for different number of variables region by region, time of day by time of day and quantile by quantile
results_5 <- readRDS('results_VarSel_5.RDS')
results_10 <- readRDS('results_VarSel_10.RDS')
results_tot <- readRDS('results_VarSel_tot.RDS')
results_Boruta <- readRDS('results_VarSel_Boruta.RDS')

results_agg <- data.frame(Date = Date(0), tod = numeric(0),
                              meanLoad = numeric(0), Load_norm = numeric(0),
                              GAM = numeric(0),
                              q0.05 = numeric(0), q0.1 = numeric(0), q0.5 = numeric(0), 
                              q0.9 = numeric(0), q0.95 = numeric(0), mean = numeric(0))
N <- length(which(results_5$tod == 0))
Var_sel_weights <- matrix(nrow = N, ncol = 3, 0)
pred_i <- data.frame(Date = Date(0), tod = numeric(0),
                     region = character(0), meanLoad = numeric(0),
                     Load_norm = numeric(0), GAM = numeric(0),
                     GAM_Lasso = numeric(0), GAM_tot = numeric(0), GAM_Boruta = numeric(0))
for (i in 0:47){
  sel_i<- which(results_5$tod == i)
  sel_i_Boruta <- which(results_Boruta$tod == i)
  experts <- as.matrix(cbind(results_5[sel_i, "q0.5"],
                                 results_10[sel_i, "q0.5"],
                                 results_tot[sel_i, "q0.5"]))
  colnames(experts) <- c("5var", "10var", "tot")
  Y <- results_5[sel_i, "residual"]
  agg <- opera::mixture(Y = Y, experts = experts, model = "MLpol", loss.gradient = TRUE, loss.type = c(name = 'pinball', tau = 0.5))
  pred_i <- rbind(pred_i, 
                  data.frame(Date = ymd(floor_date(results_5$Date[sel_i], unit = 'day')), tod = i,
                             region = results_5$region[sel_i], meanLoad = results_5$meanLoad[sel_i],
                             Load_norm = results_5$Load_norm[sel_i], GAM = results_5$GAM[sel_i],
                             GAM_Lasso = results_5$GAM[sel_i] + agg$prediction,
                             GAM_tot = results_tot$GAM[sel_i] + results_tot$q0.5[sel_i],
                             GAM_Boruta = results_Boruta$GAM[sel_i] + results_Boruta$q0.5[sel_i]))
  print(i)
}
res_1 <- filter(pred_i, Date <= ymd("2020-05-11"))
res_2 <- filter(pred_i, Date > ymd("2020-05-11"))

mape_1_lasso <- round(100*mean(abs(res_1$Load_norm - res_1$GAM_Lasso)/res_1$Load_norm),2)
mape_1_tot <- round(100*mean(abs(res_1$Load_norm - res_1$GAM_tot)/res_1$Load_norm),2)
mape_1_Boruta <- round(100*mean(abs(res_1$Load_norm - res_1$GAM_Boruta)/res_1$Load_norm),2)

mape_2_lasso <- round(100*mean(abs(res_2$Load_norm - res_2$GAM_Lasso)/res_2$Load_norm),2)
mape_2_tot <- round(100*mean(abs(res_2$Load_norm - res_2$GAM_tot)/res_2$Load_norm),2)
mape_2_Boruta <- round(100*mean(abs(res_2$Load_norm - res_2$GAM_Boruta)/res_2$Load_norm),2)

print(paste(mape_1_lasso, mape_1_Boruta, mape_1_tot))
print(paste(mape_2_lasso, mape_2_Boruta, mape_2_tot))
