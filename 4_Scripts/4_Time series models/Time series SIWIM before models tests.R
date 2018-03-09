
#Loading data SIWIM
setwd("D:/Dropbox/Data science/Formation CEPE/Projet/GitHub/SiWIM-project")
siwim_test_data_hours <- fread("2_Data/2_Retraitees/SiWIM_test_data_hours_original.csv")
siwim_test_data_hours_full <- fread("2_Data/2_Retraitees/SiWIM_test_data_hours_full.csv")


# str(siwim_test_data_hours)
# str(siwim_test_data_hours_full)
# 
# summary(siwim_test_data_hours)

#ID as number
siwim_test_data_hours[, V1 := as.numeric(siwim_test_data_hours$V1)]
siwim_test_data_hours_full[, V1 := as.numeric(siwim_test_data_hours_full$V1)]

# setnames(siwim_data_hours,"semaine_fac","Semaine_fac")
# setnames(siwim_test_data_hours_full,"semaine_fac","Semaine_fac")


# Refactor classes data
cols <- c(
  "Heure",
  "Mois_annee",
  "Jour_semaine",
  "Semaine_fac"
)

siwim_test_data_hours[, (cols) := lapply(.SD, function(x) as.factor(x)), .SDcols=cols]
siwim_test_data_hours_full[, (cols) := lapply(.SD, function(x) as.factor(x)), .SDcols=cols]
siwim_test_data_hours[, Jour_semaine_heure := NULL]

siwim_test_data_hours$Jour_semaine <- relevel(siwim_test_data_hours$Jour_semaine, ref = "lundi")
siwim_test_data_hours_full$Jour_semaine <- relevel(siwim_test_data_hours_full$Jour_semaine, ref = "lundi")

#Format date & time
siwim_test_data_hours[, time_step := as.POSIXct(time_step, format = '%Y-%m-%d %H:%M:%S')]
siwim_test_data_hours_full[, time_step := as.POSIXct(time_step, format = '%Y-%m-%d %H:%M:%S')]

siwim_test_data_hours[, Date := as.Date(Date)]
siwim_test_data_hours_full[, Date := as.Date(Date)]

siwim_test_data_hours[,Heure_num := as.numeric(Heure)]
siwim_test_data_hours_full[,Heure_num := as.numeric(Heure)]


#str(siwim_test_data_hours)
#summary(siwim_test_data_hours)

#str(siwim_test_data_hours_full)
#summary(siwim_test_data_hours_full)

period <- 24
n_weekdays <- unique(siwim_test_data_hours[, Jour_semaine])
n_date <- sort(unique(siwim_test_data_hours[, Date]))

# Data prepartion for categorical and lags data

siwim_test_data_hours <- siwim_test_data_hours[order(time_step)]

#str(siwim_test_data_hours)

# Data preparation for predictions

siwim_test_data_hours_X <- siwim_test_data_hours[, c("Count", "Heure", "Jour_semaine")]

## lags
nb_lags <- 168

lags <- paste("lag", 1:nb_lags, sep = "_")

siwim_test_data_hours[ , (lags) := shift(Count, 1:nb_lags)]
siwim_test_data_hours_X[ , (lags) := shift(siwim_test_data_hours$Count, 1:nb_lags)]

siwim_test_data_hours_X_mat <- as.data.table(model.matrix(Count~.-1, siwim_test_data_hours_X))
#nrow(siwim_test_data_hours_X_mat)
#str(siwim_test_data_hours_X_mat)

start <- nrow(siwim_test_data_hours) - nrow(siwim_test_data_hours_X_mat) +1
#start

siwim_test_data_hours_Y <- siwim_test_data_hours[start:nrow(siwim_test_data_hours)
                                                 ,"Count"]
#nrow(siwim_test_data_hours_Y)

siwim_test_data_hours_rf <- cbind(siwim_test_data_hours_Y, siwim_test_data_hours_X_mat)
