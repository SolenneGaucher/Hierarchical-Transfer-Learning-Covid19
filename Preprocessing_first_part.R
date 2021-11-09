rm(list=objects())
library(dplyr)
library(lubridate)
setwd("~/Desktop/Simulations Covid Article/data")

region_names <- c("auvergne.rhone.alpes", "bourgogne.franche.comte", "bretagne", 
                  "centre.val.de.loire", "grand.est", "hauts.de.france",
                  "ile.de.france", "normandie", "nouvelle.aquitaine",
                  "occitanie", "pays.de.la.loire", "PACA")
n_reg <- length(region_names)

# Google Mobility Indices
Data_Google <- read.csv('GoogleFrance.csv', sep = ";")
Data_Google$DateD <- dmy(Data_Google$date)
Data_Google <- filter(Data_Google, sub_region_1 != "Corsica")
Data_Google$region <- as.factor(Data_Google$sub_region_1)
levels(Data_Google$region) <- c("France", region_names[c(7,1,2,3,4)], 
                                region_names[c(5, 6, 8, 9, 10, 11, 12)])
names(Data_Google)[9:14] <- c("Retail_and_Recreation", "Grocery_and_Pharmacy", "Parks",
                              "Transit_Stations", "Workplaces", "Residential")
# Indice Oxford_Tracker
Data_Oxford_Tracker <- read.csv('CGRT.csv', sep = ';')
Data_Oxford_Tracker <- filter(Data_Oxford_Tracker, Pays == 'France')
Data_Oxford_Tracker$DateD <- dmy(Data_Oxford_Tracker$Date)

# Load regional data
Data_reg <- rbind(readRDS("Data_regional_1.RDS"), readRDS("Data_regional_2.RDS"), readRDS("Data_regional_3.RDS"))
Data_reg <- filter(Data_reg, Date < ymd("2020-09-18"))
Data_reg <- filter(Data_reg, Date > 
                     as.POSIXct(strptime("2013-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC")))

# Some Loads are missing, we interpolate them
missData1 <- which(is.na(Data_reg[,2]))[1:4]
missData2 <- which(is.na(Data_reg[,2]))[5]
missData3 <- which(is.na(Data_reg[,2]))[6]
Data_reg[missData1 ,2:13] <- 
  (Data_reg[min(missData1)-1,2:13]+Data_reg[max(missData1)+1,2:13])/2
Data_reg[missData2 ,2:13] <- 
  (Data_reg[missData2-1,2:13]+Data_reg[missData2+1,2:13])/2
Data_reg[missData3 ,2:13] <- 
  (Data_reg[missData3-1,2:13]+Data_reg[missData3+1,2:13])/2
Data_reg$DateD <- ymd(floor_date(Data_reg$Date, unit = 'day'))

# Regions are not in the same order in  the load and in the temperature
load_order <- c(12, 11, 5, 4, 1, 8, 7, 9, 2, 10, 6, 3)
temp_order <- c(10, 11, 1, 6, 12, 4, 5, 3, 7, 8, 2, 9)

# Dataframe avec une ligne par (Date, region)
Data_final <- data.frame(Data_reg[,c(1, 14, 15, 16, 17, 18, 19, 20, 131, 132, 133, 134, 135)])

# Concatenate the data for the different regions
# Initialisation
r <- 1
nobs <- dim(Data_reg)[1]

Data_final$region <- region_names[r]
print(region_names[r])
Data_final$Load <- Data_reg[, 1 + load_order[r]]
print(paste0('Load : ', names(Data_reg)[1 + load_order[r]]))
Data_final$Load.48 <- c(Data_reg[, 1 + load_order[r]][1:48], 
                            Data_reg[, 1 + load_order[r]][1:(nobs-48)])
print(paste0('Load.48 : ', names(Data_reg)[1 + load_order[r]]))
Data_final$Load.336 <- c(Data_reg[, 1 + load_order[r]][1:336], 
                             Data_reg[, 1 + load_order[r]][1:(nobs-336)])
print(paste0('Load.336 : ', names(Data_reg)[1 + load_order[r]]))
Data_final$Temp <- Data_reg[, 46 + temp_order[r]]
print(paste0('Temp : ', names(Data_reg)[46 + temp_order[r]]))
Data_final$Temp_s95 <- Data_reg[, 59 + 2*(temp_order[r]-1)]
print(paste0('Temp_s95 : ', names(Data_reg)[59 + 2*(temp_order[r]-1)]))
Data_final$Temp_s99 <- Data_reg[, 60 + 2*(temp_order[r]-1)]
print(paste0('Temp_s99 : ', names(Data_reg)[60 + 2*(temp_order[r]-1)]))
Data_final$Temp_s95_min <- Data_reg[, 83 + 4*(temp_order[r]-1)]
print(paste0('Temp_s95_min : ', names(Data_reg)[83 + 4*(temp_order[r]-1)]))
Data_final$Temp_s95_max <- Data_reg[, 84 + 4*(temp_order[r]-1)]
print(paste0('Temp_s95_max : ', names(Data_reg)[84 + 4*(temp_order[r]-1)]))
Data_final$Temp_s99_min <- Data_reg[, 85 + 4*(temp_order[r]-1)]
print(paste0('Temp_s99_min : ', names(Data_reg)[85 + 4*(temp_order[r]-1)]))
Data_final$Temp_s99_max <- Data_reg[, 86 + 4*(temp_order[r]-1)]
print(paste0('Temp_s99_max : ', names(Data_reg)[86 + 4*(temp_order[r]-1)]))

# Oxford_Tracker, Google mobility
Data_final <- left_join(Data_final, Data_Google[,9:16])
names(Data_final)[25:30] <- paste0(names(Data_final)[25:30], ".336")
Data_final[337:dim(Data_final)[1],25:30] <- 
  Data_final[1:(dim(Data_final)[1]-336), 25:30]
Data_final <- left_join(Data_final, Data_Oxford_Tracker[,2:3])

for (r in 2:n_reg){
  new_reg <- data.frame(Data_reg[,c(1, 14, 15, 16, 17, 18, 19, 20, 131, 132, 133, 134, 135)])
  new_reg$region <- region_names[r]
  print(region_names[r])
  new_reg$Load <- Data_reg[, 1 + load_order[r]]
  print(paste0('Load : ', names(Data_reg)[1 + load_order[r]]))
  new_reg$Load.48 <- c(Data_reg[, 1 + load_order[r]][1:48], Data_reg[, 1 + load_order[r]][1:(nobs-48)])
  print(paste0('Load.48 : ', names(Data_reg)[1 + load_order[r]]))
  new_reg$Load.336 <- c(Data_reg[, 1 + load_order[r]][1:336], Data_reg[, 1 + load_order[r]][1:(nobs-336)])
  print(paste0('Load.336 : ', names(Data_reg)[1 + load_order[r]]))
  new_reg$Temp <- Data_reg[, 46 + temp_order[r]]
  print(paste0('Temp : ', names(Data_reg)[46 + temp_order[r]]))
  new_reg$Temp_s95 <- Data_reg[, 59 + 2*(temp_order[r]-1)]
  print(paste0('Temp_s95 : ', names(Data_reg)[59 + 2*(temp_order[r]-1)]))
  new_reg$Temp_s99 <- Data_reg[, 60 + 2*(temp_order[r]-1)]
  print(paste0('Temp_s99 : ', names(Data_reg)[60 + 2*(temp_order[r]-1)]))
  new_reg$Temp_s95_min <- Data_reg[, 83 + 4*(temp_order[r]-1)]
  print(paste0('Temp_s95_min : ', names(Data_reg)[83 + 4*(temp_order[r]-1)]))
  new_reg$Temp_s95_max <- Data_reg[, 84 + 4*(temp_order[r]-1)]
  print(paste0('Temp_s95_max : ', names(Data_reg)[84 + 4*(temp_order[r]-1)]))
  new_reg$Temp_s99_min <- Data_reg[, 85 + 4*(temp_order[r]-1)]
  print(paste0('Temp_s99_min : ', names(Data_reg)[85 + 4*(temp_order[r]-1)]))
  new_reg$Temp_s99_max <- Data_reg[, 86 + 4*(temp_order[r]-1)]
  print(paste0('Temp_s99_max : ', names(Data_reg)[86 + 4*(temp_order[r]-1)]))
  
  # Dynamical Panels, Oxford_Tracker, Google
  new_reg <- left_join(new_reg, Data_Google[,9:16])
  names(new_reg)[25:30] <- paste0(names(new_reg)[25:30], ".336")
  new_reg[337:dim(new_reg)[1],25:30] <- new_reg[1:(dim(new_reg)[1]-336), 25:30]
  new_reg <- left_join(new_reg, Data_Oxford_Tracker[,2:3])
  Data_final <- rbind(Data_final, new_reg)
}
Data_final$region <- as.factor(Data_final$region)
Data_final$WeekDays <- as.factor(Data_final$WeekDays)
Data_final <- Data_final[,-c(9,10)]

# Load National Data
DataFrance <- readRDS("Data_Data_RTE_janv2012_oct2020_V3.RDS")
DataFrance <- as.data.frame(DataFrance)
DataFrance$region <- as.factor("France")
DataFrance <- DataFrance[, -c(3:5, 25:31)]
DataFrance$DateD <- ymd(floor_date(DataFrance$Date, unit = 'day'))

# extract Google mobility data and oxford tracker
DataFrance <- left_join(DataFrance, Data_Google[,9:16])
names(DataFrance)[23:28] <- paste0(names(DataFrance)[23:28], ".336")
DataFrance[337:dim(DataFrance)[1],23:28] <- DataFrance[1:(dim(DataFrance)[1]-336), 23:28]
DataFrance <- left_join(DataFrance, Data_Oxford_Tracker[,2:3])

Data_final <- rbind(Data_final, DataFrance)

saveRDS(Data_final, file = "Data_preprocessed.RDS")

