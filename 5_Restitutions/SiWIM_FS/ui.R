#
# SiWIM project
# 

library(shiny)

shinyUI(navbarPage("SiWIM project",
                   tabPanel("Surveillance du trafic et des ouvrages", 
                            includeMarkdown("explic.md")),
                   tabPanel("Web scraping"),
                   navbarMenu("Data analysis",
                              tabPanel("Data description"),
                              tabPanel("Some plots"),
                              tabPanel("NA-treatment")
                              ),
                   tabPanel("PCA and clustering"),
                   tabPanel("Time-series study"),
                   tabPanel("Anomali/outlier")
                   )
)
