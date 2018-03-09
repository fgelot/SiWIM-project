#### Sourcing prepare data befor modeling

source("Time series SIWIM before models.R")

################# New models with categorical data and lags ################

## Data prepartion for categorical and lags data

siwim_data_hours <- siwim_data_hours[order(time_step)]

str(siwim_data_hours)

lags <- paste("lag", 1:10, sep = "_")

siwim_data_hours[ , (lags) := shift(Count, 1:10)]

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 168*4,
                              horizon = 168,
                              fixedWindow = TRUE)

#run model in parallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)

lmFitTime <- train(Count ~  0 + Heure + Jour_semaine + Heure:Jour_semaine
                   + lag_1 + lag_2 + lag_3 + lag_4 + lag_9,
                   data = siwim_data_hours,
                   method = "lm",
                   metric = "RMSE",
                   #preProc = c("center", "scale"),
                   trControl = myTimeControl,
                   na.action = na.omit
)

summary(lmFitTime)
lmFitTime$results

lmFitTime2 <- train(Count ~  0 + Heure + Jour_semaine + Heure:Jour_semaine
                   + lag_1 + lag_2 + lag_3 + lag_4 + lag_9,
                   data = siwim_data_hours,
                   method = "lm",
                   metric = "MAPE",
                   #preProc = c("center", "scale"),
                   trControl = myTimeControl,
                   na.action = na.omit
)

# collect resamples
results <- resamples(list(lm1=lmFitTime, lm2=lmFitTime2))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

pred_test <- predict(lmFitTime, newdata = siwim_data_hours)

plot(pred_test, type="l")

## Random forest

# Data preparation 

siwim_data_hours_X <- siwim_data_hours[, c("Count", "Heure", "Jour_semaine")]

lags <- paste("lag", 1:10, sep = "_")

siwim_data_hours_X[ , (lags) := shift(siwim_data_hours$Count, 1:10)]

head(siwim_data_hours_X)

siwim_data_hours_X_mat <- as.data.table(model.matrix(Count~.-1, siwim_data_hours_X))

head(siwim_data_hours_X_mat)

siwim_data_hours_Y <- siwim_data_hours[11:nrow(siwim_data_hours)
                                       ,"Count"]

siwim_data_hours_rf <- cbind(siwim_data_hours_Y, siwim_data_hours_X_mat)

class(siwim_data_hours_X_mat)

dim(siwim_data_hours_rf)
dim(siwim_data_hours_X_mat)
head(siwim_data_hours_Y)
dim(siwim_data_hours_Y)
str(siwim_data_hours_X_mat)
str(siwim_data_hours_Y)
nrow(siwim_data_hours_X_mat) == nrow(siwim_data_hours_Y)
nrow(siwim_data_hours)

mtry <- sqrt(ncol(siwim_data_hours_X_mat))

rf_tunegrid <- expand.grid(.mtry=mtry)
summary(siwim_data_hours_Y)

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 168 * 4,
                              horizon = 168,
                              fixedWindow = TRUE)


rfFitTime <- train(Count ~.,
                   data = siwim_data_hours_rf,
                   method = "rf",
                   #preProc = c("center", "scale"),
                   tuneGrid = rf_tunegrid,
                   trControl = myTimeControl,
                   importance = T
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
saveRDS(rfFitTime, "rf_SIWIM_model.rds")


pred_test <- predict(rfFitTime, newdata = matrix_test) + trend_for$mean

mix_data_lm <- data.table( Count = c(new_count, pred_test),
                           Date = c(sort(siwim_data_hours$Date), for_range),
                           Type = c(rep("Real", nrow(siwim_data_hours)), 
                                    rep("Forecast", length(trend_for$mean))))

ggplot(mix_data_lm, aes(Date, Count, color = Type)) +
  geom_line(size = 1.2) +
  labs(title = paste(lmFitTime$modelInfo$label)) +
  theme_ts

## xgBoost

xgb_grid_1 <- expand.grid(
  nrounds= 2400,
  eta=c(0.01,0.001,0.0001),
  lambda = 1,
  alpha =0
)

################# Singular Spectrum analysis ################################

siwim_data_SSA <- ssa(siwim_data_hours$Count)

plot(siwim_data_SSA)
plot(siwim_data_SSA,type ="vector")
plot(siwim_data_SSA,type ="paired")
plot(siwim_data_SSA,type ="wcor")

#siwim_data_Cluster <- clusterify(siwim_data_SSA, group = 1:5, nclust = 3)

siwim_data_recons <- reconstruct(siwim_data_SSA, groups = list(1, c(2,3), c(4,5)))

plot(siwim_data_recons$F1)
plot(siwim_data_recons$F2)
plot(siwim_data_recons$F3)

        