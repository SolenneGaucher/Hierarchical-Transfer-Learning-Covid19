################################ One random forest at national level #####################################

rm(list=objects())
library(lubridate)
library(dplyr)
library(glmnet)
library(ranger)
library(ggpubr)
library(ALEPlot)

setwd("~/Desktop/Simulations Covid Article/data")

set.seed(42)

### - Load Data
Data <- readRDS("Data_GAM.RDS")
lockdown_start <- ymd("2020-03-16")
Data <- filter(Data, Date >= lockdown_start, Date < ymd("2020-09-17"), weights > 0, region == 'France')

### We select 4, 9, 14 variables using Lasso for each region, and use themm in the formula with the variable "WeekDays"
GAM_terms <- paste0("GAM_terms", 1:9)[-c(4,5)] # we remove terms 4 et 5, corresponding to toy and date
google_terms <- names(Data)[23:28]
form <- as.formula(paste0("residuals_norm ~ WeekDays + residuals_norm.48 + residuals_norm.336 + GovernmentResponseTracker + ",
                          paste0(GAM_terms, collapse = ' + '), " + ",
                          paste0(google_terms, collapse = ' + ')))

rf <- ranger(form, data = Data, quantreg = T, keep.inbag = TRUE)

predict_forest_q0.05 <- function(X.model, newdata)
{
  predict(X.model, data=newdata, type = "quantiles", quantiles = 0.05)$predictions
}

predict_forest_q0.5 <- function(X.model, newdata)
{
  predict(X.model, data=newdata, type = "quantiles", quantiles = 0.5)$predictions
}

predict_forest_q0.95 <- function(X.model, newdata)
{
  predict(X.model, data=newdata, type = "quantiles", quantiles = 0.95)$predictions
}

variables <- all.vars(form)[-1]
pretty_variables <- variables
pretty_variables[5:11] <-  paste("GAM_", c("Load.336", "WeekDays:DLS", "WeekDays:Load.48", "Date:Temp",
                                               "Temp_s95", "Temp_s99", "Temp_s99_min:Temp_s99_max"), sep = '')
pretty_variables[12:17] <- substr(pretty_variables[12:17],1,nchar(pretty_variables[12:17])-4)
pretty_variables[2:3] <- c("residuals.48", "residuals.336")

for (j in 1:length(variables)){
  jpeg(paste0("~/Desktop/Simulations Covid Article/images/ALE_q0.05_",pretty_variables[j],'.jpg'), width = 800, height = 500)
  ALEPlot(X=Data[,variables], X.model=rf, pred.fun=predict_forest_q0.05, J=c(16,17))
  dev.off()
  
  jpeg(paste0("~/Desktop/Simulations Covid Article/images/ALE_q0.5_",pretty_variables[j],'.jpg'), width = 800, height = 500)
  ALEPlot(X=Data[,variables], X.model=rf, pred.fun=predict_forest_q0.5, J=j)
  dev.off()
  
  jpeg(paste0("~/Desktop/Simulations Covid Article/images/ALE_q0.95_",pretty_variables[j],'.jpg'), width = 800, height = 500)
  ALEPlot(X=Data[,variables], X.model=rf, pred.fun=predict_forest_q0.95, J=j)
  dev.off()
  
  print(j)
}

x_0.05 <- list()
y_0.05 <- list()
x_0.5 <- list()
y_0.5 <- list()
x_0.95 <- list()
y_0.95 <- list()


for (i in 1:length(variables)){
  d <- variables[i]
  y_0.05[[i]] <- ALEPlot(X=Data[,variables], X.model=rf, pred.fun=predict_forest_q0.05, J=d)$f.values
  x_0.05[[i]] <- ALEPlot(X=Data[,variables], X.model=rf, pred.fun=predict_forest_q0.05, J=d)$x.values
  
  y_0.5[[i]] <- ALEPlot(X=Data[,variables], X.model=rf, pred.fun=predict_forest_q0.5, J=d)$f.values
  x_0.5[[i]] <- ALEPlot(X=Data[,variables], X.model=rf, pred.fun=predict_forest_q0.5, J=d)$x.values
  
  y_0.95[[i]] <- ALEPlot(X=Data[,variables], X.model=rf, pred.fun=predict_forest_q0.95, J=d)$f.values
  x_0.95[[i]] <- ALEPlot(X=Data[,variables], X.model=rf, pred.fun=predict_forest_q0.95, J=d)$x.values
  print(i)
}

for (i in 2:length(variables)){
  d <- variables[i]
  jpeg(paste0("~/Desktop/Simulations Covid Article/images_ALE/ALE_", pretty_variables[i],'.jpg'), width = 800, height = 500)
  plot(x = x_0.05[[i]], y = y_0.05[[i]], type = 'l', col = 'red', 
       ylim = c(min(c(y_0.05[[i]], y_0.5[[i]], y_0.95[[i]])), max(c(y_0.05[[i]], y_0.5[[i]], y_0.95[[i]]))),
       ylab = "Accumulated Local Effect", xlab = pretty_variables[i])
  lines(x = x_0.5[[i]], y = y_0.5[[i]], col = 'black')
  lines(x = x_0.95[[i]], y = y_0.95[[i]], col = 'blue')
  dev.off()
  print(i)
}


