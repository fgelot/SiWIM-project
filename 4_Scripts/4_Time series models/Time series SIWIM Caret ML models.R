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

lm_simple_FitTime <- train(Count ~  0 + Heure + Jour_semaine,
                   data = siwim_data_hours,
                   method = "lm",
                   #preProc = c("center", "scale"),
                   trControl = myTimeControl,
                   allowParallel = T,
                   na.action = na.omit
)
lm_simple_FitTime
summary(lm_simple_FitTime)

plot(lm_simple_FitTime$resample$RMSE, type = 'l', lwd = 2,
     main = "RMSE (predict)",
     xlab = "training #", ylab = "RMSE")

plot(lm_simple_FitTime$resample$Rsquared, type = 'l', lwd = 2,
     main = "R squared (predict)",
     xlab = "training #", ylab = "R²")

## Save simple model
saveRDS(lm_simple_FitTime, "lm_simple_SIWIM_model.rds")

## Simple model with interactions without lags ands MAs

lm_simple_int_FitTime <- train(Count ~  0 + Heure + Jour_semaine
                           + Heure:Jour_semaine,
                           data = siwim_data_hours,
                           method = "lm",
                           #preProc = c("center", "scale"),
                           trControl = myTimeControl,
                           allowParallel = T,
                           na.action = na.omit
)

lm_simple_int_FitTime
summary(lm_simple_int_FitTime)

plot(lm_simple_int_FitTime$resample$RMSE, type = 'l', lwd = 2,
     main = "RMSE (predict)",
     xlab = "training #", ylab = "RMSE")

plot(lm_simple_int_FitTime$resample$Rsquared, type = 'l', lwd = 2,
     main = "R squared (predict)",
     xlab = "training #", ylab = "R²")

## Save simple lm model with interactions
saveRDS(lm_simple_int_FitTime, "lm_simple_int_SIWIM_model.rds")

## Linear model with lags

lmFitTime <- train(Count ~  0 + Heure + Jour_semaine
                   + lag_1 + lag_2 + lag_4 + lag_9 + lag_15 + 
                     lag_23 + lag_27 + lag_29 + lag_45 + lag_90 + lag_95 + lag_100
                   + lag_111 + lag_143 + lag_164 + lag_168,
                   data = siwim_data_hours,
                   method = "lm",
                   #preProc = c("center", "scale"),
                   trControl = myTimeControl,
                   allowParallel = T,
                   na.action = na.omit,
                   intercept = F
)
lmFitTime
summary(lmFitTime)

plot(lmFitTime$resample$RMSE, type = 'l', lwd = 2,
     main = "RMSE (predict)",
     xlab = "training #", ylab = "RMSE")

plot(lmFitTime$resample$Rsquared, type = 'l', lwd = 2,
     main = "R squared (predict)",
     xlab = "training #", ylab = "R²")

lmFitTime$pred

## COllinearity problem from variables
length(lmFitTime$finalModel$coefficients) > lmFitTime$finalModel$rank

## Find collinear variables
df1 <- model.matrix(Count ~   0 + Heure + Jour_semaine
                    + lag_1 + lag_2 + lag_4 + lag_9 + lag_15 + 
                      lag_23 + lag_27 + lag_29 + lag_45 + lag_90 + lag_95 + lag_100
                    + lag_111 + lag_143 + lag_164 + lag_168, data = siwim_data_hours)
df2 = cor(df1)
hc = findCorrelation(df2, cutoff=0.3) # putt any value as a "cutoff" 
hc = sort(hc)
reduced_Data = df1[,-c(hc)]
print (reduced_Data)


lmFitTime$results

## Save lm with lags

saveRDS(lmFitTime, "lm_SIWIM_model.rds")

lmFitTime2 <- train(Count ~  0 + Heure + Jour_semaine #+ Heure:Jour_semaine
                   + lag_100 + lag_164,
                   data = siwim_data_hours,
                   method = "lm",
                   #preProc = c("center", "scale"),
                   trControl = myTimeControl,
                   na.action = na.omit
)

summary(lmFitTime2)

# collect resamples
results <- resamples(list(lm1=lmFitTime, lm2=lmFitTime2))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

pred_test <- predict(lmFitTime2, newdata = siwim_data_hours[, c("Heure", "Jour_semaine",
                                                                "lag_100", "lag_164")])

plot(pred_test, type="l")

## Regularized models with Caret

# Grid to tune alpha (ridge, lasso, elasticnet) and lambda

reg_tunegrid <- expand.grid(.alpha=c(0,0.5,1), .lambda = c(1,10,50,100,200,500,1000))

regFitTime <- train(Count ~  0 + Heure + Jour_semaine
                    + lag_1 + lag_2 + lag_4 + lag_9 + lag_15 + 
                      lag_23 + lag_27 + lag_29 + lag_45 + lag_90 + lag_95 + lag_100
                    + lag_111 + lag_143 + lag_164 + lag_168,
                   data = siwim_data_hours,
                   method = "glmnet",
                   #preProc = c("center", "scale"),
                   trControl = myTimeControl,
                   tuneGrid = reg_tunegrid,
                   #allowParallel = T,
                   na.action = na.omit
)

  ## Best model is Lasso
regFitTime$bestTune
regFitTime$levels

regFitTime

summary(regFitTime$bestTune)

## Save lasso model with lags

saveRDS(regFitTime, "reg_SIWIM_model.rds")

## Random forest with Caret

## Random forest Caret

mtry_optim <- sqrt(ncol(siwim_data_hours_X_mat))

rf_tunegrid <- expand.grid(.mtry=c(1,mtry_optim)) #, .ntree = c(100, 200, 500))
summary(siwim_data_hours_Y)

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 168*4,
                              horizon = 168,
                              fixedWindow = TRUE,
                              verboseIter = T,
                              allowParallel = T,
                              returnResamp = "all",
                              savePredictions = T)

rfFitTime <- train(Count ~.,
                   data = siwim_data_hours_rf,
                   method = "rf",
                   #preProc = c("center", "scale"),
                   tuneGrid = rf_tunegrid,
                   trControl = myTimeControl,
                   ntree = 500,
                   importance = T,
                   allowParallel = T
                   #na.action = na.omit
)

summary(rfFitTime)
rfFitTime$results
plot(rfFitTime)
rfFitTime$levels
imp <- varImp(rfFitTime$finalModel, scale = TRUE)

imp$var <- rownames(imp)
rownames(imp) <- NULL
imp[order(imp$Overall, decreasing = T),]

## Save Rf model
#saveRDS(rfFitTime, "rf_SIWIM_model_with_MA.rds")
saveRDS(rfFitTime, "rf_SIWIM_model.rds")

## Predict with rf model

pred_test <- predict(rfFitTime, newdata = siwim_data_hours_X_mat)
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

gbmFitTime <- train(Count ~ 0 + Heure00 + Heure01 + Heure02 + Heure03 + Heure04 + Heure05 +
                      Heure06 + Heure07 + Heure08 + Heure09 + Heure10 + Heure11 + Heure12 +
                      Heure13 + Heure14 + Heure15 + Heure16 + Heure17 + Heure18 + Heure19 +
                      Heure20 + Heure21 + Heure22 + Heure23 + Jour_semainedimanche +
                      Jour_semainemardi + Jour_semainemercredi + Jour_semainejeudi
                      + Jour_semainevendredi + Jour_semainesamedi + lag_1 + lag_2 
                      + lag_4 + lag_9 + lag_15 + 
                      lag_23 + lag_27 + lag_29 + lag_45 + lag_90 + lag_95 + lag_100
                      + lag_111 + lag_143 + lag_164 + lag_168,
                   data = siwim_data_hours_rf,
                   method = "gbm",
                   #preProc = c("center", "scale"),
                   tuneGrid = gbm_grid,
                   trControl = myTimeControl,
                   verbose = FALSE
                   #importance = T,
                   #allowParallel = T
                   #na.action = na.omit
)

 summary(gbmFitTime)

summary(gbmFitTime$finalModel$valid.error)

gbmFitTime$bestTune

gbmFitTime$results

## predict with gbm
pred_test <- predict(gbmFitTime, newdata = siwim_data_hours_X_mat)
plot(pred_test, type = "l")

## Save gbm model
saveRDS(gbmFitTime, "gbm_SIWIM_model.rds")



## xgBoost with Caret

xgb_grid <- expand.grid(
  nrounds= 2400,
  eta=c(0.01,0.001,0.0001),
  lambda = c(0,0.5,1),
  alpha =c(0,0.5,1)
)

xgbFitTime <- train(Count ~.,
                   data = siwim_data_hours_rf,
                   method = "xgbLinear",
                   #preProc = c("center", "scale"),
                   tuneGrid = xgb_grid,
                   trControl = myTimeControl,
                   verbose=F
                   #importance = T,
                   #allowParallel = T
                   #na.action = na.omit
)

summary(xgbFitTime)

################# Singular Spectrum analysis ################################

siwim_data_SSA <- ssa(siwim_data_hours$Count, neig = 5)

plot(siwim_data_SSA)
plot(siwim_data_SSA,type ="vector")
plot(siwim_data_SSA,type ="paired")
plot(siwim_data_SSA,type ="wcor")

#siwim_data_Cluster <- clusterify(siwim_data_SSA, group = 1:5, nclust = 3)

siwim_data_recons <- reconstruct(siwim_data_SSA, groups = list(1,c(2,3), c(4,5), c(6,7)))#, c(5,6), c(7,8), c(9,10)))

plot(siwim_data_recons$F1, type = "l")
plot(siwim_data_recons$F2, type = "l")
plot(siwim_data_recons$F3, type = "l")
plot(siwim_data_recons$F4, type = "l")
plot(siwim_data_recons$F5, type = "l")


plot(siwim_data_recons$F1+
       siwim_data_recons$F2+
       siwim_data_recons$F3+
       siwim_data_recons$F4+
       siwim_data_recons$F5, type = "l")

        