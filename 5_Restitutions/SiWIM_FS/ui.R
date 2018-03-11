#
# SiWIM project
# 

library(shiny)

shinyUI(navbarPage("SiWIM project",
                   tabPanel("Surveillance du trafic et des ouvrages", 
                            includeHTML("explic.html")),
                   tabPanel("Web scraping"),
                   navbarMenu("Data analysis",
                              tabPanel("Data description"),
                              tabPanel("Some plots"),
                              tabPanel("NA-treatment")
                              ),
                   tabPanel("PCA and clustering"),
                   tabPanel("Time-series study"),
                   tabPanel("Anomalie/outlier")
                   )
)
