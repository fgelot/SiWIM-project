rm(list = ls())

#Loading libraries
library(forecast)
library(randomForest) # ensemble learning method
library(caret)
library(ggplot2)
library(data.table)
library(zoo) # simple time series
#library(aTSA)
library(tseries) # Augmented Dickey-Fuller test
library(xts)# extensible time series
library(TTR) # Simple moving averages
library(Rssa) # Singular Spectrum analysis
library(dygraphs) # Nice graphs for time series
library(imputeTS) #Missing values in time series
library(doMC)

## Initial feature for ggplot2
theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))