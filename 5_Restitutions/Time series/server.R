#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dygraphs)
library(DT)
library(xts)
library(data.table)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  setwd("D:/Dropbox/Data science/Formation CEPE/Projet/")
  #source("Time series SIWIM initialization.R", local = T)
  source("Time series SIWIM before models.R", local = T)
  setwd("D:/Dropbox/Data science/Formation CEPE/Projet/")
  source("Time series SIWIM before models tests.R", local = T)
  
  
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
  
  
  startDate <- reactive({
    as.character(min(SIWIM_data()$Date))
  })
  endDate <- reactive({
    as.character(max(SIWIM_data()$Date))
  })
  
  output$dateRange <- renderUI({
    dateRangeInput(
      inputId = "idDateRange",
      label = "Modifier la plage des données : ",
      start = startDate(),
      end = endDate(),
      format = "yyyy-mm-dd",
      language = "fr",
      separator = "au"
    )
  })
  
  # observe({
  #   if (is.null(input$idDateRange))
  #     return()
  #   
  #   isolate({
  #     print(paste(as.character(input$idDateRange), collapse = " to "))
  #   })
  # })
  # 
  output$view_data <- DT::renderDataTable(datatable(
    SIWIM_data()[,c(1:8)],
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
  ) %>% formatRound(columns = c(4:8), 2))
  
  #... but not the other way around!
  dyStartDate <- reactive({
    if (!is.null(input$idDateRange[1])) {
      strftime(input$idDateRange[1])
    }
  })
  
  dyEndDate <- reactive({
    if (!is.null(input$idDateRange[2])) {
      strftime(input$idDateRange[2])
    }
  })

  
  siwim_xts <- reactive({
    if (is.null(input$idYData) || !(input$idYData %in% names(SIWIM_data()))){
        xts(SIWIM_data()[,c("Count"), drop = FALSE],
        order.by = as.POSIXct(SIWIM_data()$time_step))
    }else{
      xts(SIWIM_data()[,input$idYData, with=FALSE],
          order.by = as.POSIXct(SIWIM_data()$time_step))
    }
  })
  
  #print(head(original_xts))
  
  output$graph_data <- renderDygraph({
    dygraph(siwim_xts(), main = "Données par jour et heure") %>%
      dyOptions(drawGrid = input$showgrid) %>%
      dyRangeSelector(
                      dateWindow = c(dyStartDate(),
                                     dyEndDate()),
                      retainDateWindow = F
                      )
  })
  
})
