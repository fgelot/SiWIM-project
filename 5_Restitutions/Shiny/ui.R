#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  skin = "yellow",
  #theme = shinytheme("cerulean"),
  dashboardHeader(title = "Projet SIWIM"),
  ## Sidebar content
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem(
        "Introduction système SIWIM",
        tabName = "Introduction",
        icon = icon("truck"),
        startExpanded = TRUE
      ),
      menuSubItem("Web scraping",
                  tabName = "Web_scrap"),
      menuItem(
        "Données et gestion des valeurs manquantes",
        tabName = "Data_analysis",
        icon = icon("truck"),
        startExpanded = TRUE
      ),
      # menuSubItem("Description des données",
      #             tabName = "Data_desc"),
      # menuSubItem("Valeurs manqauntes",
      #             tabName = "NA_values"),
      menuItem(
        "ACP et Clustering",
        tabName = "ACP_Clustering",
        icon = icon("truck"),
        startExpanded = TRUE
      ),
      menuItem(
        "Prédiction temporelle",
        tabName = "Time_series",
        icon = icon("truck"),
        startExpanded = TRUE
      ),
      menuSubItem("Approche adoptée",
                  tabName = "Approach"),
      menuSubItem("Apprentissage des modèles",
                  tabName = "Training_models"),
      menuSubItem("Prédiction des modèles",
                  tabName = "Testing_models"),
      menuItem(
        "Prédiction d'anomalies",
        tabName = "Anomalies",
        icon = icon("truck"),
        startExpanded = TRUE
      ),
      menuSubItem("Anomalies avec approche univariée",
                  tabName = "univarie"),
      menuSubItem("Anomalies avec approche multivariée",
                  tabName = "multivarie"),
      menuSubItem("Anomalies avec package car",
                  tabName = "package_car")
    )
  ),
  
  dashboardBody(
  #   tags$div(
  #   tags$script("MathJax.Hub.Config({HTML-CSS: {minScaleAdjust: 200%}});")
  # ),
    tabItems(
    tabItem(tabName = "Introduction",
            htmlOutput("introHTML")),
    tabItem(tabName = "Web_scrap",
            htmlOutput("web_scrap")),
    tabItem(tabName = "Data_analysis",
            htmlOutput("data_desc")),
    tabItem(tabName = "ACP_Clustering",
            htmlOutput("ACP_clust")),
    # First tab content
    tabItem(tabName = "Time_series",
            fluidRow(
              box(
                # Choix du groupe de donnÃ©es
                radioButtons(
                  inputId = "idDataGroup",
                  label = "Sélectionner un groupe de données : ",
                  selected = 1,
                  choices = c("Apprentissage" = 1, "Tests" = 2)
                ),
              
                #Selection de la plage de dates
                # uiOutput(outputId = "dateRange"),
                # Choix des données à afficher
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
              box(#Dygraph des données
                dygraphOutput("graph_data"), width = 12)
              
            )),
    
    # Second tab content
    tabItem(tabName = "Approach",
            htmlOutput("approachHTML")),
    
    # Third tab content
    tabItem(tabName = "Training_models",
            fluidRow(
              box(
                # Choix du groupe de donnÃ©es
                radioButtons(
                  inputId = "idDataTrainGroup",
                  label = "Sélectionner un groupe de données : ",
                  selected = 1,
                  choices = c("Original" = 1, "Complété" = 2)
                ),
                
                #Selection de la plage de dates
                # uiOutput("dateRangeTrain"),
                # Choix des données à afficher
                radioButtons(
                  inputId = "idModelTrainData",
                  label = "Sélectionner les prédictions à afficher : ",
                  selected = 1,
                  choices = c(
                    "Linéaire simple" = 1,
                    "Linéaire avec interactions" = 2,
                    "Linéaire avec lags" = 3,
                    "Régularisée" = 4,
                    "Random forest" = 5,
                    "Gradient Boosting" = 6
                  )
                ),
                checkboxInput("showgridTrain", label = "Afficher la grille", value = TRUE)
                ,
                width = 4
              ),
              box(# affichage des donnees
                DT::dataTableOutput("view_results_train"),
                # ligne horizontale
                hr(), width = 8),
              box(#Dygraph des données
                dygraphOutput("graph_data_train"), width = 12),
              box(# importance des variables
                plotOutput("graph_var_imp")
                )
            )),
    
    # Fourth tab content
    tabItem(tabName = "Testing_models",
            fluidRow(
              box(
                # Choix du groupe de donnÃ©es
                radioButtons(
                  inputId = "idDataTestGroup",
                  label = "Sélectionner un groupe de données : ",
                  selected = 1,
                  choices = c("Original" = 1, "Complété" = 2)
                ),
                
                #Selection de la plage de dates
                # uiOutput("dateRangeTest"),
                # Choix des données à afficher
                radioButtons(
                  inputId = "idModelTestData",
                  label = "Sélectionner les prédictions à afficher : ",
                  selected = 1,
                  choices = c(
                    "Linéaire simple" = 1,
                    "Linéaire avec interactions" = 2,
                    "Linéaire avec lags" = 3,
                    "Régularisée" = 4,
                    "Random forest" = 5,
                    "Gradient Boosting" = 6
                  )
                ),
                checkboxInput("showgridTest", label = "Afficher la grille", value = TRUE)
                ,
                width = 4
              ),
              box(# affichage des donnees
                DT::dataTableOutput("view_results_test"),
                # ligne horizontale
                hr(), width = 8),
              box(#Dygraph des données
                dygraphOutput("graph_data_test"), width = 12)
            )
            ), 
     tabItem(tabName = "Anomalies",
              fluidRow(
                
      # Choix de la méthode pour anomalies
      radioButtons(
        inputId = "type_anomalies",
        label = "Sélectionner la méthode de calcul des anomalies : ",
        selected = 1,
        choices = c("Poids total, approche univariée" = 1, 
                    "Vitesse, approche univariée" = 2, 
                    "Essieu 2, approche univariée" = 3, 
                    "Distance de Cook, approche multivariée" = 5,
                    "Résidus, approche multivariée" = 4, 
                    "Hat values,approche multivariée" = 6, 
                    "Valeurs r-student, approche multivariée" = 7)
      ),
    box(# affichage des figures
      imageOutput("figures_anomalies"), width=12)
    )),
    tabItem(tabName = "univarie",
            htmlOutput("univarie")),
    tabItem(tabName = "multivarie",
            htmlOutput("multivarie")),
    tabItem(tabName = "package_car",
            htmlOutput("pacakge_car"))
  ))
))
