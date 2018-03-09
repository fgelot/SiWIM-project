#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(dygraphs)
library(DT)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  skin = "purple",
  #theme = shinytheme("cerulean"),
  dashboardHeader(title = "Projet SIWIM"),
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Données brutes",
      tabName = "Data_vis",
      icon = icon("dashboard")
    ),
    menuItem("Approche adoptée", tabName = "Approach", icon = icon("th")),
    menuItem(
      "Apprentissage des modèles",
      tabName = "Training_models",
      icon = icon("th")
    ),
    menuItem(
      "Prédiction des modèles",
      tabName = "Testing_models",
      icon = icon("th")
    )
  )),
  
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "Data_vis",
            fluidRow(
              # div(h1("Fréquence des camions sur la phase d'apprentissage"
              #        , style = "color: blue;")
              #     , align = "center"),
              
              box(
                # Choix du groupe de données
                radioButtons(
                  inputId = "idDataGroup",
                  label = "Sélectionner un groupe de données : ",
                  selected = 1,
                  choices = c("Apprentissage" = 1, "Tests" = 2)
                ),
                
                #Selection de la plage de dates
                uiOutput("dateRange"),
                # Choix des donnÃ©es Ã  afficher
                checkboxGroupInput(
                  inputId = "idYData",
                  label = "Sélectionner les variables à afficher : ",
                  selected = "Count"
                  ,
                  choices = c(
                    "Fréquence" = "Count",
                    "Poids" = "Total_Weight",
                    "Distance entre essieux" = "Total_axle_dist",
                    "Température moyenne" = "T_mean",
                    "Vitesse moyenne" = "Vitesse_mean"
                  )
                ),
                checkboxInput("showgrid", label = "Afficher la grille", value = TRUE)
                ,
                width = 4
              ),
              box(# affichage des donnees
                DT::dataTableOutput("view_data"),
                # ligne horizontale
                hr(), width = 8),
              box(#Dygraph des donnÃ©es
                dygraphOutput("graph_data"), width = 12)
              
            )),
    
    # Second tab content
    tabItem(tabName = "Approach",
            h2("COntenu de l'approche adopté"),
            includeMarkdown("General approach.Rmd")),
    
    # Third tab content
    tabItem(tabName = "Training_models",
            h2("COntenu de l'apprentissage des modèles")),
    
    # Fourth tab content
    tabItem(tabName = "Testing_models",
            h2("COntenu du test des modèles"))
  ))
))
