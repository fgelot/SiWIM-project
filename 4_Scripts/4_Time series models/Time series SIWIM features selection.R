#### Sourcing prepare data befor modeling

setwd("D:/Dropbox/Data science/Formation CEPE/Projet/")
source("Time series SIWIM before models.R")

## library for selection
library(leaps)

## Data prepartion for categorical and lags data

siwim_data_hours <- siwim_data_hours[order(time_step)]

str(siwim_data_hours)

## lags

nb_lags <- 168

lags <- paste("lag", 1:nb_lags, sep = "_")

## Build matrices with dummmy variables

siwim_data_hours_X <- siwim_data_hours[, c("Count", "Heure", "Jour_semaine")]

siwim_data_hours_X[ , (lags) := shift(siwim_data_hours$Count, 1:nb_lags)]

#siwim_data_hours_X_mat <- as.data.table(model.matrix(Count~.-1, siwim_data_hours_X))
# dim(siwim_data_hours_X_mat)
# head(siwim_data_hours_X_mat)

# siwim_data_hours_Y <- siwim_data_hours[11:nrow(siwim_data_hours)
                                       # ,"Count"]
# dim(siwim_data_hours_Y)
# head(siwim_data_hours_Y)

# siwim_data_hours_rf <- cbind(siwim_data_hours_Y, siwim_data_hours_X_mat)

#step()

lmfit <- lm(Count ~., data = siwim_data_hours_X)

model_step <- step(lmfit, direction = "both", k = log(nrow(siwim_data_hours_X)))

# best_model <- regsubsets(Count ~., siwim_data_hours_X, nbest = 1, method = "exhaustive", 
                         # nvmax = 30, intercept = FALSE, really.big = )

## Save model stepwise
saveRDS(model_step, "lm_SIWIM_stepwise_model.rds")

model_step$fitted.values

model_step <- readRDS("lm_SIWIM_stepwise_model.rds")

model_step$coefficients
