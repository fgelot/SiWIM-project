#### Sourcing prepare data before testing models

setwd("D:/Dropbox/Data science/Formation CEPE/Projet/")
source("Time series SIWIM before models.R")

## Load models

#original data

lm_simple_SIWIM_model     <- readRDS('4_Scripts/4_Time series models/lm_simple_SIWIM_model.rds')
lm_simple_int_SIWIM_model <- readRDS('4_Scripts/4_Time series models/lm_simple_int_SIWIM_model.rds')
lm_SIWIM_model            <- readRDS('4_Scripts/4_Time series models/lm_SIWIM_model.rds')
reg_SIWIM_model           <- readRDS('4_Scripts/4_Time series models/reg_SIWIM_model.rds')
rf_SIWIM_model            <- readRDS('4_Scripts/4_Time series models/rf_SIWIM_model.rds')
gbm_SIWIM_model           <- readRDS('4_Scripts/4_Time series models/gbm_SIWIM_model.rds')

#full data
lm_simple_full_SIWIM_model     <- readRDS('4_Scripts/4_Time series models/lm_simple_full_SIWIM_model.rds')
lm_simple_int_full_SIWIM_model <- readRDS('4_Scripts/4_Time series models/lm_simple_int_full_SIWIM_model.rds')
lm_full_SIWIM_model            <- readRDS('4_Scripts/4_Time series models/lm_full_SIWIM_model.rds')
reg_full_SIWIM_model           <- readRDS('4_Scripts/4_Time series models/reg_full_SIWIM_model.rds')
rf_full_SIWIM_model            <- readRDS('4_Scripts/4_Time series models/rf_full_SIWIM_model.rds')
gbm_full_SIWIM_model           <- readRDS('4_Scripts/4_Time series models/gbm_full_SIWIM_model.rds')


# Data prepartion for categorical and lags data

siwim_data_hours <- siwim_data_hours[order(time_step)]

str(siwim_data_hours)

# Data preparation for predictions

siwim_data_hours_X <- siwim_data_hours[, c("Count", "Heure", "Jour_semaine")]

## lags
nb_lags <- 168

lags <- paste("lag", 1:nb_lags, sep = "_")

siwim_data_hours[ , (lags) := shift(Count, 1:nb_lags)]
siwim_data_hours_X[ , (lags) := shift(Count, 1:nb_lags)]

siwim_data_hours_X_mat <- as.data.table(model.matrix(Count~.-1, siwim_data_hours_X))
nrow(siwim_data_hours_X_mat)
str(siwim_data_hours_X_mat)

start <- nrow(siwim_data_hours) - nrow(siwim_data_hours_X_mat) +1
start

siwim_data_hours_Y <- siwim_data_hours[start:nrow(siwim_data_hours)
                                       ,"Count"]
nrow(siwim_data_hours_Y)

siwim_data_hours_rf <- cbind(siwim_data_hours_Y, siwim_data_hours_X_mat)

str(siwim_data_hours_rf)

pred_train_lm_simple     <- predict(lm_simple_SIWIM_model, siwim_data_hours)
pred_train_lm_simple_int <- predict(lm_simple_int_SIWIM_model, siwim_data_hours)
pred_train_lm            <- predict(lm_SIWIM_model, siwim_data_hours)
pred_train_reg           <- predict(reg_SIWIM_model, siwim_data_hours)
pred_train_rf            <- predict(rf_SIWIM_model, siwim_data_hours_rf)
pred_train_gbm           <- predict(gbm_SIWIM_model, siwim_data_hours_rf)

str(pred_train_lm)
plot(pred_train_lm, type="l")



rbind.data.frame(accuracy(pred_train_lm_simple, siwim_data_hours$Count),
accuracy(pred_train_lm_simple_int, siwim_data_hours$Count),
accuracy(pred_train_lm, siwim_data_hours_rf$Count),
accuracy(pred_train_reg, siwim_data_hours_rf$Count),
accuracy(pred_train_rf, siwim_data_hours_rf$Count),
accuracy(pred_train_gbm, siwim_data_hours_rf$Count))
