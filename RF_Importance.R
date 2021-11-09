################################ One random forest at national level #####################################

rm(list=objects())
library(lubridate)
library(dplyr)
library(glmnet)
library(ranger)
library(ggpubr)

setwd("~/Desktop/Simulations Covid Article/data")

set.seed(42)

### - Load Data
Data <- readRDS("Data_GAM.RDS")
Data <- filter(Data, Date >= ymd("2019-09-01"), Date < ymd("2020-09-17"), weights > 0, region == 'France')

# Define usefull variables
lockdown_start <- ymd("2020-03-16")

### We select 4, 9, 14 variables using Lasso for each region, and use themm in the formula with the variable "WeekDays"
GAM_terms <- paste0("GAM_terms", 1:9)[-c(4,5)] # we remove terms 4 et 5, corresponding to toy and date
google_terms <- names(Data)[23:28]
form <- as.formula(paste0("residuals_norm ~ WeekDays + residuals_norm.48 + residuals_norm.336 + GovernmentResponseTracker + ",
                          paste0(GAM_terms, collapse = ' + '), " + ",
                          paste0(google_terms, collapse = ' + ')))
nvar = length(all.vars(form))-1
variables <- all.vars(form)[2:(nvar+1)]

# Initialise the results
forecast_date <- lockdown_start
dates <- unique(filter(Data, Date > lockdown_start + duration(1, units = "days"))$DateD)
N <- length(dates)

importance <- matrix(0, ncol = nvar, nrow = N)
colnames(importance) <- all.vars(form)[2:(nvar+1)]

## %%%%%%%%%%%%% Compute the predictions for the pandemix period %%%%%%%%%%%%%%% ##
for (j in 1:N){
  forecast_date <- dates[j]
  # Train the RF using the avalaible pandemic data, produce forecasts
  for (i in 0:47){
    sel_test <- which(Data$tod == i & Data$DateD == forecast_date)
    if (length(sel_test) != 0){
      sel_train <- which(Data$tod == i & Data$Date >= lockdown_start &  Data$Date < forecast_date)
      rf <- ranger(form, data = Data[sel_train,], quantreg = T, importance = 'impurity')
      importance[j, names(rf$variable.importance)] <- importance[j, names(rf$variable.importance)] + rf$variable.importance/48
    }
  }
  print(forecast_date)
}

# Save results
saveRDS(importance, 'importance.RDS')

################# Importance plot
importance <- readRDS('importance.RDS')
for (r in 1:N){
  importance[r,] <- 100*importance[r,]/sum(importance[r,])
}
groupes <- list(WeekDays = 1, laggued_residuals = 2:3, GovernmentResponseTracker = 4,
                GAM_effects = 5:11, Google_mobility = 12:17)
groupe_importance <- data.frame(Date = Date(0), region = character(0), type = character(0), importance = numeric(0))
for (g in 1:length(groupes)){
  if (g %in% c(1,3)){
    groupe_importance <- rbind(groupe_importance,
                               data.frame(Date = dates,
                                          type = names(groupes)[g],
                                          importance = importance[,groupes[[g]]]))
    
  }else{
    groupe_importance <- rbind(groupe_importance,
                               data.frame(Date = dates,
                                          type = names(groupes)[g],
                                          importance = rowSums(importance[,groupes[[g]]], na.rm = T)))
    
  }
}
library(ggplot2)
ggplot(data = filter(groupe_importance, Date < ymd("2020-08-01"))) +
  geom_line( aes(x = Date, y = importance, color = type, ymax = 60)) +
  theme(legend.position = "None")
#ggsave('/Users/solenne/Desktop/Simulations Covid Article/images/Goupes_importance', width = 10, height = 8)

groupe_importance$type <- as.factor(groupe_importance$type)
levels(groupe_importance$type) <- c("GAM effect", "Google mobility data", "Government Response Tracker",
                                      "Laggued residuals", "Day of week")
  
################# Google mobility importance plot
google_importance <- data.frame(Date = Date(0), region = character(0), groupe = character(0), importance = numeric(0))
for (g in groupes$Google_mobility){
  google_importance <- rbind(google_importance,
                             data.frame(Date = dates,
                                        variable = variables[g],
                                        importance = importance[,g]))
}

ggplot(data = filter(google_importance, Date < ymd("2020-08-01")), aes(x = Date, y = importance, color = variable, ymax = 13)) +
  geom_line() + theme(legend.position = "None")

################################ One random forest for all levels #####################################

rm(list=objects())
library(lubridate)
library(dplyr)
library(glmnet)
library(quantregForest)
setwd("~/Desktop/Simulations Covid Article/data")

set.seed(42)

### - Load Data
Data <- readRDS("Data_GAM.RDS")
Data <- filter(Data, Date >= ymd("2019-09-01"), Date < ymd("2020-09-17"), weights > 0)

# Define usefull variables
lockdown_start <- ymd("2020-03-16")

### We select 4, 9, 14 variables using Lasso for each region, and use themm in the formula with the variable "WeekDays"
GAM_terms <- paste0("GAM_terms", 1:9)[-c(4,5)] # we remove terms 4 et 5, corresponding to toy and date
google_terms <- names(Data)[23:28]
form <- as.formula(paste0("residuals_norm ~ WeekDays + residuals_norm.48 + residuals_norm.336 + GovernmentResponseTracker + ",
                          paste0(GAM_terms, collapse = ' + '), " + ",
                          paste0(google_terms, collapse = ' + ')))
nvar = length(all.vars(form))-1
variables <- all.vars(form)[2:(nvar+1)]

# Initialise the results
forecast_date <- lockdown_start
dates <- unique(filter(Data, Date > lockdown_start + duration(1, units = "days"))$DateD)
N <- length(dates)

importance <- matrix(0, ncol = nvar, nrow = N)
colnames(importance) <- all.vars(form)[2:(nvar+1)]

## %%%%%%%%%%%%% Compute the predictions for the pandemix period %%%%%%%%%%%%%%% ##
for (j in 1:N){
  forecast_date <- dates[j]
  # Train the RF using the avalaible pandemic data, produce forecasts
  for (i in 0:47){
    sel_test <- which(Data$tod == i & Data$DateD == forecast_date)
    if (length(sel_test) != 0){
      sel_train <- which(Data$tod == i & Data$Date >= lockdown_start &  Data$Date < forecast_date)
      rf <- ranger(form, data = Data[sel_train,], quantreg = T, importance = 'impurity')
      importance[j, names(rf$variable.importance)] <- importance[j, names(rf$variable.importance)] + rf$variable.importance/48
    }
  }
  print(forecast_date)
}

# Save results
saveRDS(importance, 'importance_common.RDS')

################# Importance plot
importance <- readRDS('importance_common.RDS')
for (r in 1:N){
  importance[r,] <- 100*importance[r,]/sum(importance[r,])
}
groupes <- list(WeekDays = 1, laggued_residuals = 2:3, GovernmentResponseTracker = 4,
                GAM_effects = 5:11, Google_mobility = 12:17)
variables[12:17] <- c('Retail and recreation', 'Grocery and pharmacy', 'Parks', 'Transit stations',
                                 'Worplaces', 'Residential')
groupe_importance <- data.frame(Date = Date(0), region = character(0), groupe = character(0), importance = numeric(0))
for (g in 1:length(groupes)){
  if (g %in% c(1,3)){
    groupe_importance <- rbind(groupe_importance,
                               data.frame(Date = dates,
                                          groupe = names(groupes)[g],
                                          importance = importance[,groupes[[g]]]))
    
  }else{
    groupe_importance <- rbind(groupe_importance,
                               data.frame(Date = dates,
                                          groupe = names(groupes)[g],
                                          importance = rowSums(importance[,groupes[[g]]], na.rm = T)))
    
  }
}
library(ggplot2)
#ggsave('/Users/solenne/Desktop/Simulations Covid Article/images/Goupes_importance', width = 10, height = 8)

groupe_importance[,"type"] <- as.factor(groupe_importance$groupe)
levels(groupe_importance[,"type"]) <- c("GAM effect", "Google mobility data", "Government Response Tracker",
                                      "Laggued residuals", "Day of week")
ggplot(data = filter(groupe_importance, Date < ymd("2020-08-01")), aes(x = Date, y = importance, color = type, ymax = 60)) +
  geom_line()

  ################# Google mobility importance plot
google_importance <- data.frame(Date = Date(0), region = character(0), groupe = character(0), importance = numeric(0))
for (g in groupes$Google_mobility){
  google_importance <- rbind(google_importance,
                             data.frame(Date = dates,
                                        variable = variables[g],
                                        importance = importance[,g]))
}
library(ggplot2)
ggplot(data = filter(google_importance, Date < ymd("2020-08-01")), aes(x = Date, y = importance, color = variable, ymax = 14)) +
  geom_line()


#############################################################################################################
############## Importance on the last day for the quantile RF at level 0.5 and 0.05 at national level #######
rm(list=objects())
set.seed(42)

### functions
pinball_loss <- function(pred, target, tau){
  l <-  (target > pred) * tau * (target - pred) +
    (pred >= target) * (1 - tau) * (pred - target)
  return(l)
}

importance_quantile <- function(form, data, qt, rep = 10){
  variables <- all.vars(form)[-1]
  target <- all.vars(form)[1]
  nvar <- length(variables)
  results <- matrix(0, ncol = nvar, nrow = length(qt))
  colnames(results) <- variables
  rownames(results) <- qt
  rf.base <- ranger(form, data = data, quantreg = T, keep.inbag = TRUE)
  pinball.base <- rep(NA, length(qt))
  for (q in 1:length(qt)){
    pinball.base[q] <- mean(pinball_loss(target = data[,target],
                                         pred = predict(rf.base, type = "quantiles", quantiles = qt[q])$predictions,
                                         tau = qt[q]))
  }
  for (v in variables){
    for (r in rep){
      data_rep <- data
      data_rep[, v] <- data[sample(nrow(data)), v]
      rf.rep <- ranger(form, data = data_rep, quantreg = TRUE, keep.inbag = TRUE)
      for (q in 1:length(qt)){
        pinball.rep <- mean(pinball_loss(target = data_rep[,target],
                                         pred = predict(rf.rep, type = "quantiles", quantiles = qt[q])$predictions,
                                         tau = qt[q]))
        results[q, v] <- results[q, v] + (pinball.rep - pinball.base[q])
      }
    }
  }
  return(results)
}


### - Load Data
Data <- readRDS("Data_GAM.RDS")
Data <- filter(Data, Date >= ymd("2019-09-01"), Date < ymd("2020-09-17"), weights > 0, region == 'France')

# Define usefull variables
lockdown_start <- ymd("2020-03-16")

### We select 4, 9, 14 variables using Lasso for each region, and use themm in the formula with the variable "WeekDays"
GAM_terms <- paste0("GAM_terms", 1:9)[-c(4,5)] # we remove terms 4 et 5, corresponding to toy and date
google_terms <- names(Data)[23:28]
form <- as.formula(paste0("residuals_norm ~ WeekDays + residuals_norm.48 + residuals_norm.336 + GovernmentResponseTracker + ",
                          paste0(GAM_terms, collapse = ' + '), " + ",
                          paste0(google_terms, collapse = ' + ')))
nvar = length(all.vars(form))-1
variables <- all.vars(form)[2:(nvar+1)]
importance <- matrix(0, ncol = nvar, nrow = 3)
names(importance) <- variables
qt <- c(0.05, 0.5, 0.95)
## %%%%%%%%%%%%% Compute the predictions for the pandemic period %%%%%%%%%%%%%%% ##
for (i in 0:47){
  sel_train <- which(Data$tod == i & Data$Date >= lockdown_start)
  importance <- importance + importance_quantile(form, Data[sel_train,], qt, rep = 100)
  print(i)
}
importance <- importance * (importance >0)
colnames(importance)[5:11] <-  paste("GAM_", c("Load_norm.336", "WeekDays:DLS", "WeekDays:Load_norm.48", "Date:Temp",
                                 "Temp_s95", "Temp_s99", "Temp_s99_min:Temp_s99_max"))
variables <- colnames(importance)
importance_df <- data_frame(importance = numeric(0), quantile = character(0), variable = character(0))
for (i in 1:3){
  importance_df <- rbind(importance_df,
                           data_frame(importance = 100*importance[i,]/sum(importance[i,]),
                                      quantile = c("0.05", "0.5", "0.95")[i],
                                      variable = variables))
}
importance_df$variable <- factor(importance_df$variable, levels = variables[order(importance[2,], decreasing = T)])
ggplot(data = importance_df, aes(x = variable, y = importance, color = quantile)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))
