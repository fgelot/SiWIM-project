#### Sourcing prepare data befor modeling

setwd("D:/Dropbox/Data science/Formation CEPE/Projet/")
source("Time series SIWIM initialization.R")
source("Time series SIWIM before models.R")

################# New models with categorical data and lags ################

## Control for cross validation time series
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 168*4,
                              horizon = 168,
                              fixedWindow = TRUE,
                              verboseIter = T,
                              allowParallel = T,
                              returnResamp = "all",
                              savePredictions = T)

## Linear models with Caret

#run model in parallel
registerDoMC(cores = detectCores() - 1)

## Simple model without lags ands MAs

lm_simple_full_FitTime <- train(Count ~  0 + Heure + Jour_semaine,
                   data = siwim_data_hours_full,
                   method = "lm",
                   #preProc = c("center", "scale"),
                   trControl = myTimeControl,
                   allowParallel = T,
                   na.action = na.omit
)
lm_simple_full_FitTime
summary(lm_simple_full_FitTime)

plot(lm_simple_full_FitTime$resample$RMSE, type = 'l', lwd = 2,
     main = "RMSE (predict)",
     xlab = "training #", ylab = "RMSE")

plot(lm_simple_full_FitTime$resample$Rsquared, type = 'l', lwd = 2,
     main = "R squared (predict)",
     xlab = "training #", ylab = "R²")

## Save simple model
saveRDS(lm_simple_full_FitTime, "lm_simple_full_SIWIM_model.rds")

## Simple model with interactions without lags ands MAs

lm_simple_int_full_FitTime <- train(Count ~  0 + Heure + Jour_semaine
                           + Heure:Jour_semaine,
                           data = siwim_data_hours_full,
                           method = "lm",
                           #preProc = c("center", "scale"),
                           trControl = myTimeControl,
                           allowParallel = T,
                           na.action = na.omit
)

lm_simple_int_full_FitTime
summary(lm_simple_int_full_FitTime)

plot(lm_simple_int_full_FitTime$resample$RMSE, type = 'l', lwd = 2,
     main = "RMSE (predict)",
     xlab = "training #", ylab = "RMSE")

plot(lm_simple_int_full_FitTime$resample$Rsquared, type = 'l', lwd = 2,
     main = "R squared (predict)",
     xlab = "training #", ylab = "R²")

## Save simple lm model with interactions
saveRDS(lm_simple_int_full_FitTime, "lm_simple_int_full_SIWIM_model.rds")

## Linear model with lags

lm_full_FitTime <- train(Count ~  0 + Heure + Jour_semaine
                   + lag_1 + lag_2 + lag_3 + lag_5 + lag_7 + lag_8 + lag_10 
                   + lag_19 + lag_23 + lag_26 + lag_30 + lag_34 + lag_43 + lag_51 + lag_110
                   + lag_120 + lag_121 + lag_131 + lag_135 + lag_139 + lag_143 
                   + lag_147 + lag_160 + lag_165 + lag_166,
                   data = siwim_data_hours_full,
                   method = "lm",
                   #preProc = c("center", "scale"),
                   trControl = myTimeControl,
                   allowParallel = T,
                   na.action = na.omit,
                   intercept = F
)
lm_full_FitTime
summary(lm_full_FitTime)

plot(lm_full_FitTime$resample$RMSE, type = 'l', lwd = 2,
     main = "RMSE (predict)",
     xlab = "training #", ylab = "RMSE")

plot(lm_full_FitTime$resample$Rsquared, type = 'l', lwd = 2,
     main = "R squared (predict)",
     xlab = "training #", ylab = "R²")

lm_full_FitTime$pred

## COllinearity problem from variables
length(lm_full_FitTime$finalModel$coefficients) > lm_full_FitTime$finalModel$rank

## Find collinear variables
df1 <- model.matrix(Count ~   0 + Heure + Jour_semaine
                    + lag_1 + lag_2 + lag_3 + lag_5 + lag_7 + lag_8 + lag_10 
                    + lag_19 + lag_23 + lag_26 + lag_30 + lag_34 + lag_43 + lag_51 + lag_110
                    + lag_120 + lag_121 + lag_131 + lag_135 + lag_139 + lag_143 
                    + lag_147 + lag_160 + lag_165 + lag_166, data = siwim_data_hours_full)
df2 = cor(df1)
hc = findCorrelation(df2, cutoff=0.3) # putt any value as a "cutoff" 
hc = sort(hc)
reduced_Data_full = df1[,-c(hc)]
print (reduced_Data_full)


lm_full_FitTime$results

## Save lm with lags

saveRDS(lm_full_FitTime, "lm_full_SIWIM_model.rds")

## lm model without colinearity

lm_full_FitTime2 <- train(Count ~  0 + Heure + Jour_semaine #+ Heure:Jour_semaine
                   + lag_34 + lag_43 + lag_51 + lag_121 + lag_131,
                   data = siwim_data_hours_full,
                   method = "lm",
                   #preProc = c("center", "scale"),
                   trControl = myTimeControl,
                   na.action = na.omit
)

summary(lm_full_FitTime2)

# collect resamples
results <- resamples(list(lm1=lm_full_FitTime, lm2=lm_full_FitTime2))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

pred_test <- predict(lm_full_FitTime2, newdata = siwim_data_hours_full[, c("Heure", "Jour_semaine",
                                                                "lag_34", "lag_43", "lag_51"
                                                                , "lag_121",  "lag_131")])

plot(pred_test, type="l")

## Regularized models with Caret

# Grid to tune alpha (ridge, lasso, elasticnet) and lambda

reg_tunegrid <- expand.grid(.alpha=c(0,0.5,1), .lambda = c(1,10,50,100,200,500,1000))

reg_full_FitTime <- train(Count ~  0 +  Heure + Jour_semaine
                          + lag_1 + lag_2 + lag_3 + lag_5 + lag_7 + lag_8 + lag_10 
                          + lag_19 + lag_23 + lag_26 + lag_30 + lag_34 + lag_43 + lag_51 + lag_110
                          + lag_120 + lag_121 + lag_131 + lag_135 + lag_139 + lag_143 
                          + lag_147 + lag_160 + lag_165 + lag_166,
                   data = siwim_data_hours_full,
                   method = "glmnet",
                   #preProc = c("center", "scale"),
                   trControl = myTimeControl,
                   tuneGrid = reg_tunegrid,
                   #allowParallel = T,
                   na.action = na.omit
)

   ## Best model is Lasso
reg_full_FitTime$bestTune
reg_full_FitTime$levels

reg_full_FitTime

summary(reg_full_FitTime$bestTune)

## Save lasso model with lags

saveRDS(reg_full_FitTime, "reg_full_SIWIM_model.rds")

## Random forest with Caret

## Random forest Caret

mtry_optim <- sqrt(ncol(siwim_data_hours_full_X_mat))

rf_tunegrid <- expand.grid(.mtry=c(1,mtry_optim)) #, .ntree = c(100, 200, 500))
summary(siwim_data_hours_full_Y)

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 168*4,
                              horizon = 168,
                              fixedWindow = TRUE,
                              verboseIter = T,
                              allowParallel = T,
                              returnResamp = "all",
                              savePredictions = T)

rf_full_FitTime <- train(Count ~.,
                   data = siwim_data_hours_full_rf,
                   method = "rf",
                   #preProc = c("center", "scale"),
                   tuneGrid = rf_tunegrid,
                   trControl = myTimeControl,
                   ntree = 500,
                   importance = T,
                   allowParallel = T
                   #na.action = na.omit
)

summary(rf_full_FitTime)
rf_full_FitTime$results
plot(rf_full_FitTime)
rfFitTime$levels
imp <- varImp(rf_full_FitTime$finalModel, scale = TRUE)

imp$var <- rownames(imp)
rownames(imp) <- NULL
imp[order(imp$Overall, decreasing = T),]

## Save Rf model
saveRDS(rf_full_FitTime, "rf_full_SIWIM_model.rds")

## Predict with rf model

pred_test <- predict(rf_full_FitTime, newdata = siwim_data_hours_full_X_mat)
plot(pred_test, type = "l")

mix_data_lm <- data.table( Count = c(new_count, pred_test),
                           Date = c(sort(siwim_data_hours$Date), for_range),
                           Type = c(rep("Real", nrow(siwim_data_hours)), 
                                    rep("Forecast", length(trend_for$mean))))

ggplot(mix_data_lm, aes(Date, Count, color = Type)) +
  geom_line(size = 1.2) +
  labs(title = paste(lmFitTime$modelInfo$label)) +
  theme_ts

## gradient boosting with Caret

gbm_grid <- expand.grid(
  n.trees= c(100,500,1000,1500),
  interaction.depth=c(1, 5, 9),
  shrinkage  = 0.1,
  n.minobsinnode = 20
)

gbm_full_FitTime <- train(Count ~ 0 + Heure00 + Heure01 + Heure02 + Heure03 + Heure04 + Heure05 +
                      Heure06 + Heure07 + Heure08 + Heure09 + Heure10 + Heure11 + Heure12 +
                      Heure13 + Heure14 + Heure15 + Heure16 + Heure17 + Heure18 + Heure19 +
                      Heure20 + Heure21 + Heure22 + Heure23 + Jour_semainedimanche +
                      Jour_semainemardi + Jour_semainemercredi + Jour_semainejeudi
                      + Jour_semainevendredi + Jour_semainesamedi 
                      + lag_1 + lag_2 + lag_3 + lag_5 + lag_7 + lag_8 + lag_10 
                      + lag_19 + lag_23 + lag_26 + lag_30 + lag_34 + lag_43 + lag_51 + lag_110
                      + lag_120 + lag_121 + lag_131 + lag_135 + lag_139 + lag_143 
                      + lag_147 + lag_160 + lag_165 + lag_166,
                   data = siwim_data_hours_full_rf,
                   method = "gbm",
                   #preProc = c("center", "scale"),
                   tuneGrid = gbm_grid,
                   trControl = myTimeControl,
                   verbose = FALSE
                   #importance = T,
                   #allowParallel = T
                   #na.action = na.omit
)

summary(gbm_full_FitTime)

summary(gbm_full_FitTime$finalModel$valid.error)

gbm_full_FitTime$bestTune

gbm_full_FitTime$results

## predict with gbm
pred_test <- predict(gbm_full_FitTime, newdata = siwim_data_hours_full_X_mat)
plot(pred_test, type = "l")

## Save gbm model
saveRDS(gbm_full_FitTime, "gbm_full_SIWIM_model.rds")
