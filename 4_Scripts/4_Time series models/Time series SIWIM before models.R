
#Loading data SIWIM
#setwd("D:/Dropbox/Data science/Formation CEPE/Projet/GitHub/SiWIM-project")
siwim_data_hours <- fread("2_Data/2_Retraitees/SiWIM_data_hours_original.csv")
siwim_data_hours_full <- fread("2_Data/2_Retraitees/SiWIM_data_hours_full.csv")


# str(siwim_data_hours)
# str(siwim_data_hours_full)
# 
# summary(siwim_data_hours)

#ID as number
siwim_data_hours[, V1 := as.numeric(siwim_data_hours$V1)]
siwim_data_hours_full[, V1 := as.numeric(siwim_data_hours_full$V1)]

# setnames(siwim_data_hours,"semaine_fac","Semaine_fac")
# setnames(siwim_data_hours_full,"semaine_fac","Semaine_fac")


# Refactor classes data
cols <- c(
  "Heure",
  "Mois_annee",
  "Jour_semaine",
  "Semaine_fac"
)

siwim_data_hours[, (cols) := lapply(.SD, function(x) as.factor(x)), .SDcols=cols]
siwim_data_hours_full[, (cols) := lapply(.SD, function(x) as.factor(x)), .SDcols=cols]
siwim_data_hours[, Jour_semaine_heure := NULL]

siwim_data_hours$Jour_semaine <- relevel(siwim_data_hours$Jour_semaine, ref = "lundi")
siwim_data_hours_full$Jour_semaine <- relevel(siwim_data_hours_full$Jour_semaine, ref = "lundi")

#Format date & time
siwim_data_hours[, time_step := as.POSIXct(time_step, format = '%Y-%m-%d %H:%M:%S')]
siwim_data_hours_full[, time_step := as.POSIXct(time_step, format = '%Y-%m-%d %H:%M:%S')]

siwim_data_hours[, Date := as.Date(Date)]
siwim_data_hours_full[, Date := as.Date(Date)]

siwim_data_hours[,Heure_num := as.numeric(Heure)]
siwim_data_hours_full[,Heure_num := as.numeric(Heure)]

# 
# str(siwim_data_hours)
# summary(siwim_data_hours)
# 
# str(siwim_data_hours_full)
# summary(siwim_data_hours_full)

period <- 24
n_weekdays <- unique(siwim_data_hours[, Jour_semaine])
n_date <- sort(unique(siwim_data_hours[, Date]))

## Data prepartion for categorical and lags data (original and full)

siwim_data_hours_full <- siwim_data_hours_full[order(time_step)]
siwim_data_hours <- siwim_data_hours[order(time_step)]

# str(siwim_data_hours)
# str(siwim_data_hours_full)

# Data preparation for Caret

siwim_data_hours_X <- siwim_data_hours[, c("Count", "Heure", "Jour_semaine")]
siwim_data_hours_full_X <- siwim_data_hours_full[, c("Count", "Heure", "Jour_semaine")]

## lags
nb_lags <- 168

lags <- paste("lag", 1:nb_lags, sep = "_")

siwim_data_hours[ , (lags) := shift(Count, 1:nb_lags)]
siwim_data_hours_X[ , (lags) := shift(siwim_data_hours$Count, 1:nb_lags)]

siwim_data_hours_full[ , (lags) := shift(Count, 1:nb_lags)]
siwim_data_hours_full_X[ , (lags) := shift(Count, 1:nb_lags)]

siwim_data_hours_X_mat <- as.data.table(model.matrix(Count~.-1, siwim_data_hours_X))
# nrow(siwim_data_hours_X_mat)
siwim_data_hours_full_X_mat <- as.data.table(model.matrix(Count~.-1, siwim_data_hours_full_X))
# nrow(siwim_data_hours_full_X_mat)

start <- nrow(siwim_data_hours) - nrow(siwim_data_hours_X_mat) +1
start_full <- nrow(siwim_data_hours_full) - nrow(siwim_data_hours_full_X_mat) +1

siwim_data_hours_Y <- siwim_data_hours[start:nrow(siwim_data_hours)
                                       ,"Count"]
siwim_data_hours_full_Y <- siwim_data_hours_full[start_full:nrow(siwim_data_hours_full)
                                                 ,"Count"]

siwim_data_hours_rf <- cbind(siwim_data_hours_Y, siwim_data_hours_X_mat)
siwim_data_hours_full_rf <- cbind(siwim_data_hours_full_Y, siwim_data_hours_full_X_mat)


# str(siwim_data_hours_rf)
# str(siwim_data_hours_full_rf)

