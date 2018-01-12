library(data.table)
library(stringr) # manipulation de chaine de caracteres
library(missMDA) # imputation des donn√©es manquantes
library(questionr)

#Loading formated data
siwim_data <- fread("2_Data/2_Retraitees/SiWIM_data_formated.csv")

# Delete some useless features
siwim_data[, c("Stage_trace", "Offset", "Impact_factor") := NULL]

# Refactor the lane (0 = droite, 1 = gauche)
siwim_data[, Lane := factor(siwim_data$Lane, labels = c("droite", "gauche"))]

# Refactor classes data
siwim_data[, Subclass_ID := factor(siwim_data$Subclass_ID)]

# Refactor classes data
siwim_data[, Axle_groups := factor(siwim_data$Axle_groups)]

# Refactor number of axles
siwim_data[, Nb_axles := factor(siwim_data$N)]

# 14316 trucks with weights to 0, but other features are filled

# NUmber of levels for classes

# nlevels(siwim_data$Axle_groups)
# nlevels(siwim_data$Subclass_ID)

# Proportion of warning flags

prop.table(table(siwim_data$Warning_flags))

# Extract data from timestamp : date, time and POSIX format

siwim_data[, Date    := as.POSIXct(substr(siwim_data$Timestamp,1,10), format = '%Y-%m-%d')]
siwim_data[, Horaire := substr(siwim_data$Timestamp,12,19)]
siwim_data[, Timestamp := as.POSIXct(substr(siwim_data$Timestamp,1,19), format = '%Y-%m-%d %H:%M:%S')]

# Quantify time series from the 1st January 2017

siwim_data[, Time_num := as.numeric((siwim_data$Timestamp - as.POSIXct("2017-01-01 00:00:00", format = '%Y-%m-%d %H:%M:%S'))/360)]

# Create time series groups
siwim_data[, Annee := format(as.Date(siwim_data$Date), format = "%Y")]
siwim_data[, Mois_num := format(as.Date(siwim_data$Date), format = "%m")]
siwim_data[, Mois_annee := format(as.Date(siwim_data$Date), format = "%B")]
siwim_data[, Jour_num := format(as.Date(siwim_data$Date), format = "%d")]
siwim_data[, Jour_semaine := format(as.Date(siwim_data$Date), format = "%A")]
siwim_data[, Heure := str_sub(Horaire, 1, 2)]

#Create a feature for anomalie
siwim_data[,Anomalie := "N"][Warning_flags != "00000000",Anomalie := "Y"]

# Refactor anomalie
siwim_data[, Anomalie := factor(siwim_data$Anomalie)]

# plot(density(siwim_data$Time_num))

# Compute speed into km/h

siwim_data[, Vitesse := siwim_data$v * (36/10)]

# Compute global weights in Tonnes

siwim_data[, MGV := siwim_data$WGV / 9.81]
# 
# boxplot(siwim_data$Vitesse)
# 
# plot(density(siwim_data$Vitesse))
# 
# plot(x = siwim_data$Subclass_ID, y = siwim_data$Vitesse)
# 
# 
# plot(density(siwim_data$MGV))

# 
# plot(x = siwim_data$MGV, y = siwim_data$N)

# 
# boxplot(siwim_data$MGV)

# Compute all axle weights in Tonnes

maxN <- 16
axles_load <- paste("W", 1:maxN, sep = "")
axles_mass <- paste("M", 1:maxN, sep = "")
axles_load
siwim_data[, (axles_mass) := lapply(axles_load, function(x){get(x)/9.81})]

# Suppress V1 index before writing data into csv file

siwim_data[, V1 := NULL]

#Suppress old features
siwim_data [, c("v","WGV", "W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
         "W11","W12","W13","W14","W15","W16") := NULL]

###################### Analyse de valeurs aberrantes
#NUmber of trucks with weight equal to 0
siwim_data[MGV == 0, .N]

# Trucks with more than 8 axles
siwim_data[N > 8, .N]

# Trucks with total axle distance more than 20 meters
siwim_data[total_axle_dist > 20, .N]

# Records wih negative temperature
siwim_data[T < -20, .N]

# Trucks with speed more than 130 km/h
siwim_data[Vitesse > 130, .N]
siwim_data[Vitesse > 130]

# Trucks with global weight more than 70 t
siwim_data[MGV > 70, .N]
siwim_data[MGV > 70]

###################### Gestion des donnÈes aberrantes

# On travaille sur le tableau simplifiÈ quantitatif
# IdÈe gÈnÈrale : attribuer aux donn√©es aberrantes (dont nous sommes surs qu'elles soient fausses) un valeur "NA"
# puis reconstituer leur valeur grasse aux techniques d'imputation du package "missMDA"

# on considËre comme valeur anormale au dessus de 8 essieux (ici 2843 valeurs dans ce cas)
siwim_data[siwim_data$N > 8, N := NA]
siwim_data[is.na(siwim_data$N), Nb_axles := NA]

# on considËre comme valeur anormale une distance totale entre les axes sup√©rieure √† 20 m(ici 5102 valeurs dans ce cas)
siwim_data[total_axle_dist > 20, total_axle_dist := NA]

# on considËre comme valeur anormale les temp√©ratures n√©gatives < √† -10¬∞C (dans le jeu de donn√©es seule -273 est pr√©sente comme valeur n√©gative, 2014 valeurs dans ce cas)
siwim_data[T < -20, T := NA]

# # on considËre comme valeur anormale la vitesse d'un camion > 130 km/h (19 valeurs dans ce cas)
# index_aberrant_Vitesse <- which(siwim_data[,"Vitesse"] > 130)
# siwim_data[index_aberrant_Vitesse,"Vitesse"] <- NA
# 
# # on considËre comme valeur anormale la masse d'un camion > 70 T (22 valeurs dans ce cas)
# index_aberrant_MGV <- which(siwim_data[,"MGV"] > 70)
# siwim_data[index_aberrant_MGV,"MGV"] <- NA


#Suppress features linked a too big number of axles
siwim_data [, c("A8", "A9","A10",
                "A11","A12","A13","A14","A15","M9","M10",
                "M11","M12","M13","M14","M15","M16") := NULL]

df <- freq(siwim_data$Warning_flags)
df[order(df$n, decreasing = T),]
df <- sort(table(siwim_data$Subclass_ID), decreasing = T)
df2 <- freq(siwim_data$Subclass_ID)
df3 <- df2[order(df2$n, decreasing = T),]
df3


plot(df3$`%`, type = "h", lwd = 3)

str(siwim_data)

siwim_data_imput <- siwim_data[, c( "Lane", "Nb_axles", "Anomalie", "total_axle_dist", "T", 
   "Reduced_chi_squared", "Time_num", "Vitesse", "MGV")]


# siwim_data_imput <- siwim_data[, -c("N", "Timestamp", "Date", "Horaire", "Annee", 
#                                     "Mois_num", "Mois_annee", "Jour_num", "Jour_semaine", "Heure", "Axle_groups", "Warning_flags", "Subclass_ID", "Site_ID",
#                                     "A1",  "A2", 
#                                     "A3", "A4", "A5", "A6", "A7",                 
#                                     "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8")]

# on cherche ‡ prÈsent ‡ imputer les donnÈes manquantes en utilisant l'AFM
colnames(siwim_data_imput)
colorder <- c( "Lane", "Nb_axles", "Anomalie", "total_axle_dist", "T", 
               "Reduced_chi_squared", "Time_num", "Vitesse", "MGV")

siwim_data_imput <- setcolorder(siwim_data_imput, colorder)

#Estimation du nombre d'axes plantÈ (problËme mÈmoire)
nb_axe_imput <- estim_ncpFAMD(siwim_data_imput)


#Inputation of missing values from FAMD with 7 axes
siwim_data_inputted <- imputeFAMD(siwim_data_imput, ncp = 7, method = "Regularized")

summary(siwim_data_inputted$completeObs)

siwim_data_inputted <- as.data.frame(siwim_data_inputted$completeObs)

#Merge of inputted columns with initial columns
siwim_data_inputted_final <- cbind(siwim_data[, -c( "Lane", "Nb_axles", "Anomalie", "total_axle_dist", "T", 
                "Reduced_chi_squared", "Time_num", "Vitesse", "MGV")],siwim_data_inputted)

#globql checks for inputted values
class(siwim_data_inputted_final)
summary(siwim_data)
summary(siwim_data_inputted_final)

table(siwim_data_inputted_final$N)

## Other data tables where missing values are deleted
siwim_data_missing_del <- siwim_data[!is.na(siwim_data$N)
                                            & !is.na(siwim_data$total_axle_dist) 
                                            & !is.na(siwim_data$T)]

summary(siwim_data_missing_del)

# Show data

head(siwim_data)

str(siwim_data)

summary(siwim_data)

write.csv(siwim_data, file = "2_Data/2_Retraitees/SiWIM_data_prepared.csv")

write.csv(siwim_data_inputted_final, file = "2_Data/2_Retraitees/SiWIM_data_after_input.csv")

write.csv(siwim_data_missing_del, file = "2_Data/2_Retraitees/SiWIM_data_missing_deleted.csv")

