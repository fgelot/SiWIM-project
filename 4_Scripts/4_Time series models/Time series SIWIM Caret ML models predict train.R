#### Sourcing prepare data before testing models

# library(data.table)
# library(forecast)
# library(caret)
# library(gbm)
# setwd("D:/Dropbox/Data science/Formation CEPE/Projet/New_GitHub/SiWIM-project")
# source("4_Scripts/4_Time series models/Time series SIWIM before models.R")

## Load models

#original data

lm_simple_SIWIM_model     <-
  readRDS('4_Scripts/4_Time series models/Models/lm_simple_SIWIM_model.rds')
lm_simple_int_SIWIM_model <-
  readRDS('4_Scripts/4_Time series models/Models/lm_simple_int_SIWIM_model.rds')
lm_SIWIM_model            <-
  readRDS('4_Scripts/4_Time series models/Models/lm_SIWIM_model.rds')
reg_SIWIM_model           <-
  readRDS('4_Scripts/4_Time series models/Models/reg_SIWIM_model.rds')
rf_SIWIM_model            <-
  readRDS('4_Scripts/4_Time series models/Models/rf_SIWIM_model.rds')
gbm_SIWIM_model           <-
  readRDS('4_Scripts/4_Time series models/Models/gbm_SIWIM_model.rds')

#full data
lm_simple_full_SIWIM_model     <-
  readRDS('4_Scripts/4_Time series models/Models/lm_simple_full_SIWIM_model.rds')
lm_simple_int_full_SIWIM_model <-
  readRDS('4_Scripts/4_Time series models/Models/lm_simple_int_full_SIWIM_model.rds')
lm_full_SIWIM_model            <-
  readRDS('4_Scripts/4_Time series models/Models/lm_full_SIWIM_model.rds')
reg_full_SIWIM_model           <-
  readRDS('4_Scripts/4_Time series models/Models/reg_full_SIWIM_model.rds')
rf_full_SIWIM_model            <-
  readRDS('4_Scripts/4_Time series models/Models/rf_full_SIWIM_model.rds')
gbm_full_SIWIM_model           <-
  readRDS('4_Scripts/4_Time series models/Models/gbm_full_SIWIM_model.rds')


# Data prepartion for categorical and lags data

siwim_data_hours <- siwim_data_hours[order(time_step)]

#str(siwim_data_hours)

# Data preparation for predictions

siwim_data_hours_X <-
  siwim_data_hours[, c("Count", "Heure", "Jour_semaine")]

## lags
nb_lags <- 168

lags <- paste("lag", 1:nb_lags, sep = "_")

siwim_data_hours[, (lags) := shift(Count, 1:nb_lags)]
siwim_data_hours_X[, (lags) := shift(Count, 1:nb_lags)]

siwim_data_hours_X_mat <-
  as.data.table(model.matrix(Count ~ . - 1, siwim_data_hours_X))
# nrow(siwim_data_hours_X_mat)
# str(siwim_data_hours_X_mat)

start <- nrow(siwim_data_hours) - nrow(siwim_data_hours_X_mat) + 1
# start

siwim_data_hours_Y <- siwim_data_hours[start:nrow(siwim_data_hours)
                                       , "Count"]
# nrow(siwim_data_hours_Y)

siwim_data_hours_rf <-
  cbind(siwim_data_hours_Y, siwim_data_hours_X_mat)

#str(siwim_data_hours_rf)

pred_train_lm_simple     <-
  predict(lm_simple_SIWIM_model, siwim_data_hours)
pred_train_lm_simple_int <-
  predict(lm_simple_int_SIWIM_model, siwim_data_hours)
pred_train_lm            <-
  predict(lm_SIWIM_model, siwim_data_hours)
pred_train_reg           <-
  predict(reg_SIWIM_model, siwim_data_hours)
pred_train_rf            <-
  predict(rf_SIWIM_model, siwim_data_hours_rf)
pred_train_gbm           <-
  predict(gbm_SIWIM_model, siwim_data_hours_rf)

# str(pred_train_lm)
# plot(pred_train_lm, type="l")


# Predict with models full
pred_train_full_lm_simple     <-
  predict(lm_simple_full_SIWIM_model, siwim_data_hours)
pred_train_full_lm_simple_int <-
  predict(lm_simple_int_full_SIWIM_model, siwim_data_hours)
pred_train_full_lm            <-
  predict(lm_full_SIWIM_model, siwim_data_hours)
pred_train_full_reg           <-
  predict(reg_full_SIWIM_model, siwim_data_hours)
pred_train_full_rf            <-
  predict(rf_full_SIWIM_model, siwim_data_hours_rf)
pred_train_full_gbm           <-
  predict(gbm_full_SIWIM_model, siwim_data_hours_rf)

# length(pred_train_full_rf)

# siwim_data_hours[169:nrow(siwim_data_hours), "Count"]

pred_global <-
  cbind.data.frame(
    pred_train_lm_simple[169:length(pred_train_lm_simple)],
    pred_train_lm_simple_int[169:length(pred_train_lm_simple_int)],
    pred_train_lm,
    pred_train_reg,
    pred_train_rf,
    pred_train_gbm
  )

pred_global_full <-
  cbind.data.frame(
    pred_train_full_lm_simple[169:length(pred_train_full_lm_simple)],
    pred_train_full_lm_simple_int[169:length(pred_train_full_lm_simple_int)],
    pred_train_full_lm[3:length(pred_train_full_lm)],
    pred_train_full_reg[3:length(pred_train_full_reg)],
    pred_train_full_rf,
    pred_train_full_gbm
  )


results_train <-
  rbind.data.frame(
    "Simple linear" = accuracy(pred_train_lm_simple, siwim_data_hours$Count),
    "Linear with interactions" = accuracy(pred_train_lm_simple_int, siwim_data_hours$Count),
    "Linear with lags" = accuracy(pred_train_lm, siwim_data_hours_rf$Count),
    "Regularized" = accuracy(pred_train_reg, siwim_data_hours_rf$Count),
    "Random forest" = accuracy(pred_train_rf, siwim_data_hours_rf$Count),
    "Gradient Boosting" = accuracy(pred_train_gbm, siwim_data_hours_rf$Count)
  )

results_train_full <-
  rbind.data.frame(
    "Simple linear" = accuracy(pred_train_full_lm_simple, siwim_data_hours$Count),
    "Linear with interactions" = accuracy(pred_train_full_lm_simple_int, siwim_data_hours$Count),
    "Linear with lags" = accuracy(pred_train_full_lm, siwim_data_hours_rf$Count),
    "Regularized" = accuracy(pred_train_full_reg, siwim_data_hours_rf$Count),
    "Random forest" = accuracy(pred_train_full_rf, siwim_data_hours_rf$Count),
    "Gradient Boosting" = accuracy(pred_train_full_gbm, siwim_data_hours_rf$Count)
  )


 get_importance <- function(model, scale = T){

  if(model$method == "gbm"){
    importance <- relative.influence(model$finalModel, 
                                     n.trees = model$finalModel$n.trees,
                                     sort. = TRUE,
                                     scale. = scale)
    imp_df = data.frame(Overall = importance)
  }else{
    importance <- varImp(model, scale = scale)
    imp_df <- importance$importance
  }
  imp_df$vars <- rownames(imp_df)
  rownames(imp_df) <- NULL
  
  imp_df <- imp_df[order(-imp_df$Overall),]
  imp_df <- imp_df[1:30,]
  
  return(imp_df)  
}

imp_simple <- get_importance(lm_simple_SIWIM_model)
imp_simple_int <- get_importance(lm_simple_int_SIWIM_model)
imp_lags <- get_importance(lm_SIWIM_model)
imp_reg <- get_importance(reg_SIWIM_model)
imp_rf <- get_importance(rf_SIWIM_model)
imp_gbm <- get_importance(gbm_SIWIM_model)

imp_gbm

imp_simple_full <- get_importance(lm_simple_full_SIWIM_model)
imp_simple_int_full <- get_importance(lm_simple_int_full_SIWIM_model)
imp_lags_full <- get_importance(lm_full_SIWIM_model)
imp_reg_full <- get_importance(reg_full_SIWIM_model)
imp_rf_full <- get_importance(rf_full_SIWIM_model)
imp_gbm_full <- get_importance(gbm_full_SIWIM_model)

plot_importance <- function(imp, model_type){
  d <- ggplot(imp, aes(x=reorder(vars,Overall),y=Overall),size=2)
  d <- d + geom_bar(stat = "identity", aes(fill = Overall))
  d <- d  +theme(axis.text.x = element_text(vjust=1,angle=90)) 
  d <- d  + labs(x="Variable",y="Overall Importance",title= paste(model_type,"Features Importance"))
  d <- d +  coord_flip()
  #d <- d + plotTheme()
 return(d)
}

# plot_importance(imp_simple, "Simple linear")
# plot_importance(imp_simple_int, "Linear with interactions")
# plot_importance(imp_lags, "Linear with lags")
# plot_importance(imp_reg, "Regularized")
# plot_importance(imp_rf, "Random forest")
# plot_importance(imp_gbm, "Gradient Boosting")
# 
# 
# plot_importance(imp_simple_full, "Simple linear")
# plot_importance(imp_simple_int_full, "Linear with interactions")
# plot_importance(imp_lags_full, "Linear with lags")
# plot_importance(imp_reg_full, "Regularized")
# plot_importance(imp_rf_full, "Random forest")
# plot_importance(imp_gbm_full, "Gradient Boosting")

# rownames(results_train)[6]

