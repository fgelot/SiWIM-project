#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #print(getwd())
  output$introHTML <-
    renderUI({
      includeHTML("5_Restitutions/Shiny/html/explic.html")
    })
  
  output$data_desc <-
    renderUI({
      includeHTML("5_Restitutions/Shiny/html/1_Description_Donnees_Gestion_Donnees_Manquantes.html")
    })
  
  
  output$ACP_clust <-
    renderUI({
      includeHTML("5_Restitutions/Shiny/html/2_ACP_Clustering.html")
    })
  
  
  output$approachHTML <-
    renderUI({
      includeHTML("5_Restitutions/Shiny/html/General_approach.html")
    })
  
  SIWIM_data <- reactive({
    if (input$idDataGroup == 1) {
      siwim_data_hours[, c(
        "Date",
        "Heure",
        "Jour_semaine",
        "Count",
        "Total_Weight",
        "Total_axle_dist",
        "T_mean",
        "Vitesse_mean",
        "time_step"
      )]
    } else{
      siwim_test_data_hours[, c(
        "Date",
        "Heure",
        "Jour_semaine",
        "Count",
        "Total_Weight",
        "Total_axle_dist",
        "T_mean",
        "Vitesse_mean",
        "time_step"
      )]
    }
  })
  
  observe({
    if (is.null(input$idDataGroup)) {
      return()
    }
    
    isolate({
      # startDate <- as.character(min(SIWIM_data()$Date))
      # endDate <- as.character(max(SIWIM_data()$Date))
      print(min(SIWIM_data()$Date))
    })
  })
  
  
  # startDate <- reactive({
  #   if (!is.null(input$idDataGroup)) {
  #     min(SIWIM_data()$Date)
  #   }
  # })
  # endDate <- reactive({
  #   if (!is.null(input$idDataGroup)) {
  #     max(SIWIM_data()$Date)
  #   }
  # })
  
  # output$dateRange <- renderUI({
  #   # showReactLog()
  #   dateRangeInput(
  #     "idDateRange",
  #     label = "Modifier la plage des donnees : ",
  #     start =  startDate(),
  #     end = endDate(),
  #     format = "yyyy-mm-dd",
  #     language = "fr",
  #     separator = " au "
  #   )
  # })
  # 
  #... but not the other way around!
  dyStartDate <- reactive({
    if (!is.null(input$idDataGroup)) {
      strftime(min(SIWIM_data()$Date))
    }
  })
  
  dyEndDate <- reactive({
    if (!is.null(input$idDataGroup)) {
      strftime(max(SIWIM_data()$Date))
    }
  })
  
  
  # observe({
  #   if (is.null(input$idYData)){
  #     return()
  #      }
  #
  #   isolate({
  #     print(input$idDataGroup)
  #     })
  #   })
  
  output$view_data <- DT::renderDataTable(
    datatable(
      SIWIM_data()[, c(1:8)],
      colnames = c(
        'Date',
        'Heure',
        'Jour',
        'Fréquence',
        'Poids',
        "Distance d'essieux",
        'Température moyenne',
        'Vitesse moyenne'
      ),
      options = list(language = "fr")
    ) %>% formatRound(columns = c(4:8), 2)
  )
  
  
  siwim_xts <- reactive({
    input$idDataGroup
    if (is.null(input$idYData) ||
        !(input$idYData %in% names(SIWIM_data()))) {
      xts(SIWIM_data()[, c("Count"), with = FALSE],
          order.by = as.POSIXct(SIWIM_data()$time_step))
    } else{
      xts(SIWIM_data()[, input$idYData, with = FALSE],
          order.by = as.POSIXct(SIWIM_data()$time_step))
    }
  })
  
  #print(head(original_xts))
  
  output$graph_data <- renderDygraph({
    dygraph(siwim_xts(), main = "Données par jour et heure") %>%
      dyOptions(drawGrid = input$showgrid) %>%
      dyRangeSelector(dateWindow = c(dyStartDate(),
                                     dyEndDate()),
                      retainDateWindow = F)
  })
  
  ########### Prédictions sur les données d'apprentissage #######################
  
  results_train_set <- reactive({
    if (input$idDataTrainGroup == 1) {
      results_train
    } else{
      results_train_full
    }
  })
  
  
  imp_var <- reactive({
    if (input$idDataTrainGroup == 1) {
      if(input$idModelTrainData == 1){
        imp_simple
      }else if(input$idModelTrainData == 2){
        imp_simple_int
      }else if(input$idModelTrainData == 3){
        imp_lags
      }else if(input$idModelTrainData == 4){
        imp_reg
      }else if(input$idModelTrainData == 5){
        imp_rf
      }else{
        imp_gbm
      }
    } else{
      if(input$idModelTrainData == 1){
        imp_simple_full
      }else if(input$idModelTrainData == 2){
        imp_simple_int_full
      }else if(input$idModelTrainData == 3){
        imp_lags_full
      }else if(input$idModelTrainData == 4){
        imp_reg_full
      }else if(input$idModelTrainData == 5){
        imp_rf_full
      }else{
        imp_gbm_full
      }
    }
  })
  
  SIWIM_data_train <- reactive({
    if (input$idDataTrainGroup == 1) {
      siwim_data_hours[169:nrow(siwim_data_hours), c("Date",
                                                     "Heure",
                                                     "Jour_semaine",
                                                     "Count",
                                                     "time_step")]
    } else{
      siwim_data_hours_full[169:nrow(siwim_data_hours_full), c("Date",
                                                               "Heure",
                                                               "Jour_semaine",
                                                               "Count",
                                                               "time_step")]
    }
  })
  
  predictions_train <- reactive({
    if (input$idDataTrainGroup == 1) {
      pred_global
    } else {
      pred_global_full
    }
  })
  
  # startTrainDate <- reactive({
  #   as.character(min(SIWIM_data_train()$Date))
  # })
  # endTrainDate <- reactive({
  #   as.character(max(SIWIM_data_train()$Date))
  # })
  
  # output$dateRangeTrain <- renderUI({
  #   input$idDataTrainGroup
  #   isolate({
  #     dateRangeInput(
  #       inputId = "idDateRangeTrain",
  #       label = "Modifier la plage des données : ",
  #       start = min(SIWIM_data_train()$Date),
  #       end = max(SIWIM_data_train()$Date),
  #       min = min(SIWIM_data_train()$Date),
  #       max = max(SIWIM_data_train()$Date),
  #       format = "yyyy-mm-dd",
  #       language = "fr",
  #       separator = "au"
  #     )
  #   })
  # })
  # 
  
  #... but not the other way around!
  dyStartTrainDate <- reactive({
    if (!is.null(input$idDataTrainGroup)) {
      strftime(min(SIWIM_data_train()$Date))
    }
  })
  
  dyEndTrainDate <- reactive({
    if (!is.null(input$idDataTrainGroup)) {
      strftime(max(SIWIM_data_train()$Date))
    }
  })
  
  ## Résultats des modèles
  output$view_results_train <-
    DT::renderDataTable(
      datatable(
        results_train_set(),
        rownames = TRUE,
        colnames = colnames(results_train_set()),
        options = list(language = "fr")
      ) %>% formatRound(columns = c(1:5), 2)
    )
  
  
  observe({
    if (is.null(input$idModelTrainData))
      return()
    
    isolate({
      print(dim(results_train_set()))
      print(dim(SIWIM_data_train()))
      print(input$idModelTrainData)
    })
  })
  
  ## Importance des variables 
  
  output$graph_var_imp <- renderPlot(
    print(plot_importance(imp_var(), 
                    rownames(results_train_set())[as.integer(input$idModelTrainData)])
          )
  )
  
  ## GRaph des prédictions sur train data
  output$graph_data_train <- renderDygraph({
    dygraph(xts(
      x = cbind(predictions_train()[, as.integer(input$idModelTrainData)],
                SIWIM_data_train()[, c("Count"), with = FALSE]),
      order.by = as.POSIXct(SIWIM_data_train()$time_step)
    ),
    main = "Réel vs prédictions pour l'apprentissage") %>%
      dyOptions(drawGrid = input$showgridTrain) %>%
      dyRangeSelector(
        dateWindow = c(dyStartTrainDate(),
                       dyEndTrainDate()),
        retainDateWindow = F
      )
  })
  
  
  
  ########### Prédictions sur les données de test #######################
  
  results_test_set <- reactive({
    if (input$idDataTestGroup == 1) {
      results_test
    } else{
      results_test_full
    }
  })
  
  SIWIM_data_test <- reactive({
    if (input$idDataTestGroup == 1) {
      siwim_test_data_hours[169:nrow(siwim_test_data_hours), c("Date",
                                                     "Heure",
                                                     "Jour_semaine",
                                                     "Count",
                                                     "time_step")]
    } else{
      siwim_test_data_hours_full[169:nrow(siwim_test_data_hours_full), c("Date",
                                                               "Heure",
                                                               "Jour_semaine",
                                                               "Count",
                                                               "time_step")]
    }
  })
  
  predictions_test <- reactive({
    if (input$idDataTestGroup == 1) {
      pred_test_global
    } else {
      pred_test_global_full
    }
  })
  
  # startTestDate <- reactive({
  #   as.character(min(SIWIM_data_test()$Date))
  # })
  # endTestDate <- reactive({
  #   as.character(max(SIWIM_data_test()$Date))
  # })
  
  # output$dateRangeTest <- renderUI({
  #   input$idDataTestGroup
  #   isolate({
  #     dateRangeInput(
  #       inputId = "idDateRangeTest",
  #       label = "Modifier la plage des données : ",
  #       start = min(SIWIM_data_test()$Date),
  #       end = max(SIWIM_data_test()$Date),
  #       min = min(SIWIM_data_test()$Date),
  #       max = max(SIWIM_data_test()$Date),
  #       format = "yyyy-mm-dd",
  #       language = "fr",
  #       separator = "au"
  #     )
  #   })
  # })
  # 
  
  #... but not the other way around!
  dyStartTestDate <- reactive({
    if (!is.null(input$idDataTestGroup)) {
      strftime(min(SIWIM_data_test()$Date))
    }
  })
  
  dyEndTestDate <- reactive({
    if (!is.null(input$idDataTestGroup)) {
      strftime(max(SIWIM_data_test()$Date))
    }
  })
  
  ## Résultats des modèles
  output$view_results_test <-
    DT::renderDataTable(
      datatable(
        results_test_set(),
        rownames = TRUE,
        colnames = colnames(results_test_set()),
        options = list(language = "fr")
      ) %>% formatRound(columns = c(1:5), 2)
    )
  
  
  observe({
    if (is.null(input$idModelTestData))
      return()
    
    isolate({
      print(head(SIWIM_data_test()))
    })
  })
  
  
  ## GRaph des prédictions sur test data
  output$graph_data_test <- renderDygraph({
    dygraph(xts(
      x = cbind(predictions_test()[, as.integer(input$idModelTestData)],
                SIWIM_data_test()[, c("Count"), with = FALSE]),
      order.by = as.POSIXct(SIWIM_data_test()$time_step)
    ),
    main = "Réel vs prédictions pour l'apprentissage") %>%
      dyOptions(drawGrid = input$showgridTest) %>%
      dyRangeSelector(
        dateWindow = c(dyStartTestDate(),
                       dyEndTestDate()),
        retainDateWindow = F
      )
  })
  
})
