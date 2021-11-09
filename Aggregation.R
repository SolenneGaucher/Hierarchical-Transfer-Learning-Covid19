rm(list=objects())
library(lubridate)
library(dplyr)
library(opera)
library(zoo)
setwd("~/Desktop/Simulations Covid Article/data")

# Load previous results
results_ind <- readRDS('results_individual_RF.RDS')[,-8]
names(results_ind)[8:12] <-  paste0("ind_",names(results_ind)[8:12])
results_com <- readRDS('results_com_RF.RDS')[,-c(7,8)]
names(results_com)[7:11] <-  paste0("com_",names(results_com)[7:11])
results <- full_join(results_ind, results_com)
results[, 8:17] <- results[, 8:17] + results$GAM
results$aggregation <- NA

# usefull variables
regions <- levels(results$region)
n_reg <- length(regions)
lockdown_start <- ymd("2020-03-16")
quantiles <- c("q0.05", "q0.1", "q0.5", "q0.90", "q0.95")
models <- names(results[c(6, 8:17)])

# save errors 
mape <- matrix(ncol = 3, nrow = 6)
rownames(mape) <- c("GAM", "Stacked q0.5", "Hierarchical aggregation, scaled", "Hierarchical aggregation, unscaled",
                    "Full aggregation", "Vectorial aggregation")

rmse <- matrix(ncol = 3, nrow = 6)
rownames(rmse) <- c("GAM", "Stacked q0.5", "Hierarchical aggregation, scaled", "Hierarchical aggregation, unscaled",
                    "Full aggregation", "Vectorial aggregation")


################################ -  Hierarchical aggregation, scaled - ################################
# First step : obtain regional predictions:
#  aggregate 11 experts : GAM + 10 stacked RF (regional or common training set, 5 quantiles)
#  region by region
results_HAS <- results
# Aggregated predictions at the regional level
weights_HA_quantiles_for_reg <- data_frame(region = character(0), model = character(0), Date = Date(0), weights = numeric(0))

for (i in 0:47){
  for (r in 1:12){
    sel_r_i <- which((results_HAS$region == regions[r]) & (results_HAS$tod == i))
    experts_r_i <- as.matrix(results_HAS[sel_r_i, c(6, 8:17)])
    awake <- matrix(1, nrow = length(sel_r_i), ncol = 11)
    awake[which(results_HAS$Date[sel_r_i] < lockdown_start), 2:11] <- 0
    Y_r_i <- results_HAS[sel_r_i, "Load_norm"]
    agg <- opera::mixture(Y = Y_r_i, experts = experts_r_i, awake = awake, model = "MLpol", loss.gradient = TRUE)
    results_HAS$aggregation[sel_r_i] <- agg$prediction
    for (q in 1:11){
      weights_HA_quantiles_for_reg <- rbind(weights_HA_quantiles_for_reg,
                                            data_frame(region = regions[r], model = models[q],
                                                       Date = results_HAS[sel_r_i, "Date"] + duration(results_HAS[sel_r_i, "tod"]/2, "hours"),
                                                       weights = agg$weights[,q]))
    }
  }
  print(i)
}

# Second step : agregate regional predictions + national experts 

nat_results_HAS <- filter(results_HAS, region == 'France')[, c("Date", "tod", "Load_norm", "meanLoad", "GAM",
                                                       "ind_q0.05", "ind_q0.1", "ind_q0.5", "ind_q0.90", "ind_q0.95",
                                                       "com_q0.05", "com_q0.1", "com_q0.5", "com_q0.90", "com_q0.95")]
weights_HA_reg_for_nat  <- data_frame(region = character(0), Date = Date(0), weights = numeric(0))

for (r in 1:12){
  pred_r <- results_HAS[which(results_HAS$region == regions[r]),c("Date", "tod", "aggregation")]
  names(pred_r)[3] <- regions[r]
  nat_results_HAS <- left_join(nat_results_HAS, pred_r)
}
nat_results_HAS$aggregation <- NA

for (i in 0:47){
  sel_i <- which(nat_results_HAS$tod == i)
  experts_i <- as.matrix(nat_results_HAS[sel_i,5:27])
  Y_i <- c(nat_results_HAS[sel_i,"Load_norm"])
  agg <- opera::mixture(Y = Y_i, experts = experts_i, model = "MLpol", loss.gradient = TRUE)
  nat_results_HAS[sel_i,"aggregation"] <- agg$prediction
  for (r in 1:n_reg){
    weights_HA_reg_for_nat <- rbind(weights_HA_reg_for_nat,
                                          data_frame(region = regions[r],
                                                     Date = results_HAS[sel_r_i, "Date"] + duration(results_HAS[sel_r_i, "tod"]/2, "hours"),
                                                     weights = agg$weights[,r]))
  }
  print(i)
}


nat_results_HAS$aggregation <- nat_results_HAS$aggregation * nat_results_HAS$meanLoad
nat_results_HAS$GAM <- nat_results_HAS$GAM * nat_results_HAS$meanLoad
nat_results_HAS$ind_q0.5 <- nat_results_HAS$ind_q0.5 * nat_results_HAS$meanLoad
nat_results_HAS$Load <- nat_results_HAS$Load_norm * nat_results_HAS$meanLoad

res_0 <- filter(nat_results_HAS, Date < lockdown_start)
res_1 <- filter(nat_results_HAS, Date >= lockdown_start, Date < ymd("2020-05-11"))
res_2 <- filter(nat_results_HAS, Date >= ymd("2020-05-12"))

mape[1,1] <- round(100*mean(abs(res_0$Load - res_0$GAM)/res_0$Load),2)
mape[2,1] <- round(100*mean(abs(res_0$Load - res_0$ind_q0.5)/res_0$Load),2)
mape[3,1] <- round(100*mean(abs(res_0$Load - res_0$aggregation)/res_0$Load),2)

mape[1,2] <- round(100*mean(abs(res_1$Load - res_1$GAM)/res_1$Load),2)
mape[2,2] <- round(100*mean(abs(res_1$Load - res_1$ind_q0.5)/res_1$Load),2)
mape[3,2] <- round(100*mean(abs(res_1$Load - res_1$aggregation)/res_1$Load),2)

mape[1,3] <- round(100*mean(abs(res_2$Load - res_2$GAM)/res_2$Load),2)
mape[2,3] <- round(100*mean(abs(res_2$Load - res_2$ind_q0.5)/res_2$Load),2)
mape[3,3] <- round(100*mean(abs(res_2$Load - res_2$aggregation)/res_2$Load),2)

rmse[1,1] <- sqrt(mean((res_0$Load - res_0$GAM)**2))
rmse[2,1] <- sqrt(mean((res_0$Load - res_0$ind_q0.5)**2))
rmse[3,1] <- sqrt(mean((res_0$Load - res_0$aggregation)**2))

rmse[1,2] <- sqrt(mean((res_1$Load - res_1$GAM)**2))
rmse[2,2] <- sqrt(mean((res_1$Load - res_1$ind_q0.5)**2))
rmse[3,2] <- sqrt(mean((res_1$Load - res_1$aggregation)**2))

rmse[1,3] <- sqrt(mean((res_2$Load - res_2$GAM)**2))
rmse[2,3] <- sqrt(mean((res_2$Load - res_2$ind_q0.5)**2))
rmse[3,3] <- sqrt(mean((res_2$Load - res_2$aggregation)**2))

################################ -  Hierarchical aggregation, unscaled - ################################
# First step : obtain regional predictions:
#  aggregate 11 experts : GAM + 10 stacked RF (regional or common training set, 5 quantiles)
#  region by region
results_HAUS <- results

for (i in 0:47){
  for (r in 1:n_reg){
    sel_r_i <- which((results_HAUS$region == regions[r]) & (results_HAUS$tod == i))
    experts_r_i <- as.matrix(results_HAUS[sel_r_i, c(6, 8:17)])
    awake <- matrix(1, nrow = length(sel_r_i), ncol = 11)
    awake[which(results_HAUS$Date[sel_r_i] < lockdown_start), 2:11] <- 0
    Y_r_i <- results_HAUS[sel_r_i, "Load_norm"]
    agg <- opera::mixture(Y = Y_r_i, experts = experts_r_i, awake = awake, model = "MLpol", loss.gradient = TRUE)
    results_HAUS$aggregation[sel_r_i] <- agg$prediction
    }
  print(i)
}

# Second step : multiply the predictions at the regional level by the average load, add them to predict load at the national level
nat_results_HAUS <- filter(results_HAUS, region == 'France')[, c("Date", "tod", "Load_norm", "meanLoad", "GAM", "aggregation")]
nat_results_HAUS$hierarchical_agg_no_scale <- 0
true_weights <- data_frame(region = character(0),  Date = Date(0), weights = numeric(0))

for (r in 1:12){
  pred_r <- results_HAUS[which(results_HAUS$region == regions[r]),]
  pred_fr <- results_HAUS[which(results_HAUS$region == 'France'),]
  true_weights <- rbind(true_weights,
                        data_frame(region = regions[r],
                                   Date = pred_r$Date + duration(pred_r$tod/2, "hours"),
                                   weights = (pred_r$Load_norm * pred_r$meanLoad)/(pred_fr$Load_norm * pred_fr$meanLoad)))
  pred_r$hierarchical_agg_no_scale_temp <- pred_r$aggregation * pred_r$meanLoad
  pred_r <- pred_r[, c("Date", "tod", "hierarchical_agg_no_scale_temp")]
  nat_results_HAUS <- left_join(nat_results_HAUS, pred_r)
  nat_results_HAUS$hierarchical_agg_no_scale <- nat_results_HAUS$hierarchical_agg_no_scale_temp + nat_results_HAUS$hierarchical_agg_no_scale
  nat_results_HAUS <- nat_results_HAUS[,-8]
}

head(nat_results_HAUS)
nat_results_HAUS$Load <- nat_results_HAUS$Load_norm * nat_results_HAUS$meanLoad

res_0 <- filter(nat_results_HAUS, Date < lockdown_start)
res_1 <- filter(nat_results_HAUS, Date >= lockdown_start, Date < ymd("2020-05-11"))
res_2 <- filter(nat_results_HAUS, Date >= ymd("2020-05-12"))

mape[4,1] <- round(100*mean(abs(res_0$Load - res_0$hierarchical_agg_no_scale)/res_0$Load),2)
mape[4,2] <- round(100*mean(abs(res_1$Load - res_1$hierarchical_agg_no_scale)/res_1$Load),2)
mape[4,3] <- round(100*mean(abs(res_2$Load - res_2$hierarchical_agg_no_scale)/res_2$Load),2)

rmse[4,1] <- sqrt(mean((res_0$Load - res_0$hierarchical_agg_no_scale)**2))
rmse[4,2] <- sqrt(mean((res_1$Load - res_1$hierarchical_agg_no_scale)**2))
rmse[4,3] <- sqrt(mean((res_2$Load - res_2$hierarchical_agg_no_scale)**2))

################################ - Full aggregation - ################################
# Fully desagregated prediction
results_FA <- results
nat_results_FA <- filter(results_FA, region == 'France')[, c("Date", "tod", "Load_norm", "meanLoad")]
nat_results_FA$aggregation <- NA

weights_FA_regions <- data_frame(region = character(0), Date = Date(0), weights = numeric(0))
weights_FA_quantiles <- data_frame(model = character(0), Date = Date(0), weights = numeric(0))

for (i in 0:47){
  experts_i <- filter(results_FA, tod == i,region == 'France')[, c("Date", "Load_norm")]
  awake_i <- filter(results_FA, tod == i,region == 'France')[, c("Date", "tod")]
  Y_i <- experts_i$Load_norm
  for (r in 1:n_reg){
    experts_r_i<- filter(results_FA, tod == i, region == regions[r])[,c(1, 6, 8:12, 14:18)]
    names(experts_r_i)[2:12] <- paste(names(experts_r_i)[2:12], regions[r])
    experts_i <- left_join(experts_i, experts_r_i)
    awake_r_i <- experts_r_i
    awake_r_i[,2:12] <- 1
    awake_r_i[which(awake_r_i$Date < lockdown_start), 3:12] <- 0
    awake_r_i$tod <-i
    awake_i <- left_join(awake_i, awake_r_i)
  }
  experts_i <- as.matrix(experts_i[, 3:145])
  awake_i <- as.matrix(awake_i[, 3:145])
  agg <- opera::mixture(Y = Y_i, experts = experts_i, awake = awake_i, model = "MLpol",loss.gradient = TRUE)
  nat_results_FA[which(nat_results_FA$tod == i), "aggregation"] <- agg$prediction
  for (r in 1:n_reg){
    weights_FA_regions <- rbind(weights_FA_regions,
                                data_frame(region = regions[r],
                                           Date = results_FA[sel_r_i, "Date"] + duration(results_FA[sel_r_i, "tod"]/2, "hours"),
                                           weights = rowSums(agg$weights[,(11*(r-1)+1):(11*r)])))
  }
  for (q in 1:11){
    weights_FA_quantiles <- rbind(weights_FA_quantiles,
                                data_frame(model = models[q],
                                           Date = results_FA[sel_r_i, "Date"] + duration(results_FA[sel_r_i, "tod"]/2, "hours"),
                                           weights = rowSums(agg$weights[,(0:12)*11 + q])))
  }
  print(i)
}
nat_results_FA$Load <- nat_results_FA$Load_norm * nat_results_FA$meanLoad
nat_results_FA$aggregation <- nat_results_FA$aggregation * nat_results_FA$meanLoad

res_0 <- filter(nat_results_FA, Date < lockdown_start)
res_1 <- filter(nat_results_FA, Date >= lockdown_start, Date < ymd("2020-05-11"))
res_2 <- filter(nat_results_FA, Date >= ymd("2020-05-12"))

mape[5,1] <- round(100*mean(abs(res_0$Load - res_0$aggregation)/res_0$Load),2)
mape[5,2] <- round(100*mean(abs(res_1$Load - res_1$aggregation)/res_1$Load),2)
mape[5,3] <- round(100*mean(abs(res_2$Load - res_2$aggregation)/res_2$Load),2)

rmse[5,1] <- sqrt(mean((res_0$Load - res_0$aggregation)**2))
rmse[5,2] <- sqrt(mean((res_1$Load - res_1$aggregation)**2))
rmse[5,3] <- sqrt(mean((res_2$Load - res_2$aggregation)**2))

################################ - Vectorial  aggregation - ################################
# First step : obtain regional predictions:
#   aggregate 11 experts : GAM + 10 stacked RF (regional or common training set, 5 quantiles)
#   with wieght sharing accross region and national level

results_VA <- results
weights_VA <- data_frame(model = character(0), Date = Date(0), weights = numeric(0))

for (i in 0:47){
  experts_i <- array(NA, dim = c(nrow(filter(results_VA, region == 'France', tod == i)), n_reg, 11), dimnames = list(NULL, NULL, names(results_VA)[c(6, 8:17)]))
  Y_i <- matrix(NA, nrow = c(nrow(filter(results_VA, region == 'France', tod == i))), ncol = n_reg)
  awake <- array(1, dim = c(nrow(filter(results_VA, region == 'France', tod == i)), n_reg, 11))
  for (r in 1:n_reg){
    experts_i[,r,] <- as.matrix(filter(results_VA, region == regions[r], tod == i)[,c(6, 8:17)])
    Y_i[, r] <- filter(results_VA, region == regions[r], tod == i)$Load_norm
    awake[,r, 2:11] <- filter(results_VA, region == regions[r], tod == i)$Date >= lockdown_start
  }
  
  agg <- opera::mixture(Y = Y_i, experts = experts_i, awake = awake, model = "MLpol",loss.gradient = TRUE)
  
  for (r in 1:n_reg){
    sel_r_i <- which(results_VA$region == regions[r] & results_VA$tod == i)
    results_VA$aggregation[sel_r_i] <- agg$prediction[,r]
  }
  for (q in 1:11){
    weights_VA <- rbind(weights_VA,
                        data_frame(model = models[q],
                                   Date = results_VA[sel_r_i, "Date"] + duration(results_VA[sel_r_i, "tod"]/2, "hours"),
                                   weights = agg$weights[,q]))
  }
  print(i)
}

nat_results_VA <- filter(results_VA, region == 'France')[, c("Date", "tod", "Load_norm", "meanLoad", "aggregation")]
nat_results_VA$aggregation <- nat_results_VA$aggregation * nat_results_VA$meanLoad
nat_results_VA$Load <- nat_results_VA$Load_norm * nat_results_VA$meanLoad

res_0 <- filter(nat_results_VA, Date < lockdown_start)
res_1 <- filter(nat_results_VA, Date >= lockdown_start, Date < ymd("2020-06-11"))
res_2 <- filter(nat_results_VA, Date >= ymd("2020-06-12"))

mape[6,1] <- round(100*mean(abs(res_0$Load - res_0$aggregation)/res_0$Load),2)
mape[6,2] <- round(100*mean(abs(res_1$Load - res_1$aggregation)/res_1$Load),2)
mape[6,3] <- round(100*mean(abs(res_2$Load - res_2$aggregation)/res_2$Load),2)

rmse[6,1] <- sqrt(mean((res_0$Load - res_0$aggregation)**2))
rmse[6,2] <- sqrt(mean((res_1$Load - res_1$aggregation)**2))
rmse[6,3] <- sqrt(mean((res_2$Load - res_2$aggregation)**2))

rmse <- round(rmse)

########################### - Plot weights - ############################################
reg_weights <- weights_HA_reg_for_nat
reg_weights$weight_type <- "hierarchical aggregation"
true_weights$weight_type <- "true weight"
reg_weights <- rbind(reg_weights, true_weights)
weights_FA_regions$weight_type <- "fully desaggregated"
reg_weights <- rbind(reg_weights, weights_FA_regions)
reg_weights$weight_type <- as.factor(reg_weights$weight_type)
ggplot(data = reg_weights) +
  geom_boxplot(aes(x = region, y = weights, color = weight_type), outlier.shape = NA) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

model_weights <- weights_FA_quantiles
model_weights$type <- "fully desaggregated"
weights_HA_quantiles_for_reg$type <- "hierarchical aggregation"
weights_HA_quantiles_for_reg <- weights_HA_quantiles_for_reg[,2:5]
weights_VA$type <- "vectorial aggregation"
model_weights <- rbind(model_weights, weights_HA_quantiles_for_reg)
model_weights <- rbind(model_weights, weights_VA)

ggplot(data = filter(model_weights, Date >= ymd("2020-03-16"))) +
  geom_boxplot(aes(x = model, y = weights, color = type), outlier.shape = NA) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))



ggplot(data = filter(weights_FA_regions, Date >= ymd("2020-03-16"))) +
  geom_boxplot(aes(x = quantile, y = weights), outlier.shape = NA)

########################### - Normalized GAM errors for the different regions - ##########################

results$ape <- abs(results$Load_norm - results$GAM)/results$Load_norm
results$Date_full <- results$Date + duration(results$tod/2, units = "hours")
results <- results[order(results$Date_full),]
results$mape <- NA
for (r in 1:n_reg){
  sel_r <- which(results$region == regions[r])
  results$mape[sel_r] <-  rollmean(results$ape[sel_r], 336, align = "right", na.pad = T)
}
results$region <- as.character(results$region)
results[results$region == "grand.est",]$region <-  "Grand-Est"
results[results$region == 'auvergne.rhone.alpes',]$region <-  "Auvergne-Rhône-Alpes"
results[results$region == 'PACA',]$region <-  "Provence-Alpes-Côte d'Azur"
results[results$region == "occitanie",]$region <-  "Occitanie"
results[results$region == 'normandie',]$region <-  "Normandie"
results$region <- as.factor(results$region)
regions <- levels(results$region)
plot_regions <- c(1, 5, 6, 9, 11, 13)
results$Date <- results$Date_full
results$MAPE <- 100*results$mape
ggplot(data = filter(results, Date >= ymd("2020-01-01"), Date < ymd("2020-08-01"), region %in% regions[plot_regions]),
       aes(x = Date, y = MAPE, color = region)) +
  geom_line()
