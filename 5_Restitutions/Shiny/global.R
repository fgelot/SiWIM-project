## Librairies

## Server
library(shiny)
library(dygraphs)
library(DT)
library(xts)
library(data.table)
library(forecast)

## UI
library(shinythemes)
library(shinydashboard)
# library(shinyjs)

# options(shiny.reactlog=TRUE)
# options(shiny.trace=TRUE)
# options(shiny.fullstacktrace=TRUE)
# options("encoding" = "UTF-8")

## Chargement des données et modèles

setwd("D:/Dropbox/Data science/Formation CEPE/Projet/New_GitHub/SiWIM-project")
#source("Time series SIWIM initialization.R", local = T)
source("4_Scripts/4_Time series models/Time series SIWIM before models.R", local = T)
#setwd("D:/Dropbox/Data science/Formation CEPE/Projet/")
source("4_Scripts/4_Time series models/Time series SIWIM before models tests.R", local = T)
source("4_Scripts/4_Time series models/Time series SIWIM Caret ML models predict train.R", local = T)
source("4_Scripts/4_Time series models/Time series SIWIM Caret ML models testing.R", local = T)
