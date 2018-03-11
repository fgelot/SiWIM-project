
#### Sourcing prepare data before testing models

#setwd("D:/Dropbox/Data science/Formation CEPE/Projet/")
#source("4_Scripts/4_Time series models/Time series SIWIM initialization.R")
#source("4_Scripts/4_Time series models/Time series SIWIM before models tests.R")

## Load models

#original data

# lm_simple_SIWIM_model     <- readRDS('4_Scripts/4_Time series models/lm_simple_SIWIM_model.rds')
# lm_simple_int_SIWIM_model <- readRDS('4_Scripts/4_Time series models/lm_simple_int_SIWIM_model.rds')
# lm_SIWIM_model            <- readRDS('4_Scripts/4_Time series models/lm_SIWIM_model.rds')
# reg_SIWIM_model           <- readRDS('4_Scripts/4_Time series models/reg_SIWIM_model.rds')
# rf_SIWIM_model            <- readRDS('4_Scripts/4_Time series models/rf_SIWIM_model.rds')
# gbm_SIWIM_model           <- readRDS('4_Scripts/4_Time series models/gbm_SIWIM_model.rds')
# 
# #full data
# lm_simple_full_SIWIM_model     <- readRDS('4_Scripts/4_Time series models/lm_simple_full_SIWIM_model.rds')
# lm_simple_int_full_SIWIM_model <- readRDS('4_Scripts/4_Time series models/lm_simple_int_full_SIWIM_model.rds')
# lm_full_SIWIM_model            <- readRDS('4_Scripts/4_Time series models/lm_full_SIWIM_model.rds')
# reg_full_SIWIM_model           <- readRDS('4_Scripts/4_Time series models/reg_full_SIWIM_model.rds')
# rf_full_SIWIM_model            <- readRDS('4_Scripts/4_Time series models/rf_full_SIWIM_model.rds')
# gbm_full_SIWIM_model           <- readRDS('4_Scripts/4_Time series models/gbm_full_SIWIM_model.rds')



#str(siwim_test_data_hours_rf)

pred_test_lm_simple     <- predict(lm_simple_SIWIM_model, siwim_test_data_hours)
pred_test_lm_simple_int <- predict(lm_simple_int_SIWIM_model, siwim_test_data_hours)
pred_test_lm            <- predict(lm_SIWIM_model, siwim_test_data_hours)
pred_test_reg           <- predict(reg_SIWIM_model, siwim_test_data_hours)
pred_test_rf            <- predict(rf_SIWIM_model, siwim_test_data_hours_rf)
pred_test_gbm           <- predict(gbm_SIWIM_model, siwim_test_data_hours_rf)

#str(pred_test_lm)
# plot(pred_test_lm, type="l")

# Predict with models full
pred_test_full_lm_simple     <- predict(lm_simple_full_SIWIM_model, siwim_test_data_hours)
pred_test_full_lm_simple_int <- predict(lm_simple_int_full_SIWIM_model, siwim_test_data_hours)
pred_test_full_lm            <- predict(lm_full_SIWIM_model, siwim_test_data_hours)
pred_test_full_reg           <- predict(reg_full_SIWIM_model, siwim_test_data_hours)
pred_test_full_rf            <- predict(rf_full_SIWIM_model, siwim_test_data_hours_rf)
pred_test_full_gbm           <- predict(gbm_full_SIWIM_model, siwim_test_data_hours_rf)


pred_test_global <-
  cbind.data.frame(
    pred_test_lm_simple[169:length(pred_test_lm_simple)],
    pred_test_lm_simple_int[169:length(pred_test_lm_simple_int)],
    pred_test_lm,
    pred_test_reg,
    pred_test_rf,
    pred_test_gbm
  )

pred_test_global_full <-
  cbind.data.frame(
    pred_test_full_lm_simple[169:length(pred_test_full_lm_simple)],
    pred_test_full_lm_simple_int[169:length(pred_test_full_lm_simple_int)],
    pred_test_full_lm[3:length(pred_test_full_lm)],
    pred_test_full_reg[3:length(pred_test_full_reg)],
    pred_test_full_rf,
    pred_test_full_gbm
  )


results_test <- rbind.data.frame("Simple linear" = accuracy(pred_test_lm_simple, siwim_test_data_hours$Count),
                 "Linear with interactionss" = accuracy(pred_test_lm_simple_int, siwim_test_data_hours$Count),
                 "Linear with lags" = accuracy(pred_test_lm, siwim_test_data_hours_rf$Count),
                 "Regularized" = accuracy(pred_test_reg, siwim_test_data_hours_rf$Count),
                 "Random forest" = accuracy(pred_test_rf, siwim_test_data_hours_rf$Count),
                 "Gradient boosting" = accuracy(pred_test_gbm, siwim_test_data_hours_rf$Count))

results_test_full <- rbind.data.frame("Simple linear" = accuracy(pred_test_full_lm_simple, siwim_test_data_hours$Count),
                 "Linear with interactions" = accuracy(pred_test_full_lm_simple_int, siwim_test_data_hours$Count),
                 "Linear with lags" = accuracy(pred_test_full_lm, siwim_test_data_hours_rf$Count),
                 "Regularized" = accuracy(pred_test_full_reg, siwim_test_data_hours_rf$Count),
                 "Random forest" = accuracy(pred_test_full_rf, siwim_test_data_hours_rf$Count),
                 "Gradient boosting" = accuracy(pred_test_full_gbm, siwim_test_data_hours_rf$Count))
