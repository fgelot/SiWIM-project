#### Sourcing prepare data befor modeling

setwd("D:/Dropbox/Data science/Formation CEPE/Projet/")
source("Time series SIWIM initialization.R")
source("Time series SIWIM before models.R")

## library for selection
library(leaps)

#step

lm_full_fit <- lm(Count ~., data = na.omit(siwim_data_hours_full_X))

model_full_step <- step(lm_full_fit, direction = "both", k = log(nrow(siwim_data_hours_full_X)))


# best_model <- regsubsets(Count ~., siwim_data_hours_X, nbest = 1, method = "exhaustive", 
                         # nvmax = 30, intercept = FALSE, really.big = )

## Save model stepwise
saveRDS(model_full_step, "lm_full_SIWIM_stepwise_model.rds")
# 
# model_full_step$fitted.values
# 
# model_full_step$coefficients
