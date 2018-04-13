#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)

source("classification.r")
setwd(getwd())


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Alarm Flood Analysis by Imagineers"),
   tags$head(tags$script(src = "message-handler.js")),
   actionButton('doStep1', 'Classification'),
   actionButton('doStep2', 'STEP 2'),
   actionButton('doStep3', 'Clustering'),
   actionButton('doStep4', 'STEP 4'),
   actionButton('doStep6', 'STEP 6'),
   sidebarLayout(
     DT::dataTableOutput("t1"),
     # Show a plot of the generated distribution
     mainPanel(
       helpText("Pie chart of flood input data"),
       plotOutput("pc")
     )
   ),
   sidebarLayout(
     DT::dataTableOutput("t2"),
     mainPanel(
       helpText("Confusion Matrix - Training data"),
       verbatimTextOutput('cmtr')
     )
   ),
   sidebarLayout(
     DT::dataTableOutput("t3"),
     mainPanel(
       helpText("Confusion Matrix - Test data"),
       verbatimTextOutput('cmte')
     )
   ),
   sidebarLayout(
     DT::dataTableOutput("t4"),
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("distPlot")
     )
   ),
   tags$div(class = "result")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  observeEvent(input$doStep1, {
    
    output$pc <- renderPlot({
      pie(pie_1,pie_2)
    })
    
    output$cmtr <- renderPrint({
      tab1
    })
    
    output$cmte <- renderPrint({
      tab2
    })
    
    #session$sendCustomMessage(type = 'testmessage',message = 'STEP 1 result')
  })
  observeEvent(input$doStep2, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'STEP 2 result')
  })
  observeEvent(input$doStep3, {
    A<- matrix( 
      c(2, 4, 3, 1, 5, 7,
        56,3,4,89,5,6,
        5,2,4,1,1,1,6,7
        ,34,2,7,8,56), # the data elements 
      nrow=5,              # number of rows 
      ncol=5,              # number of columns 
      byrow = TRUE) 
    
    clusters <- hclust(dist(A))

    output$distPlot <- renderPlot({
      plot(clusters)
    })
    
    clusterCut <- cutree(clusters, 2)
    # output$table <- DT::renderDataTable({ 
    #   clusterCut <- cutree(clusters, 2)
    #   #table(clusterCut)
    # })
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Output of clustering result')
  })
  observeEvent(input$doStep4, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'STEP 4 result')
  })
  observeEvent(input$doStep5, {
    
    session$sendCustomMessage(type = 'testmessage',
                              message = 'STEP 5 result')
  })
  observeEvent(input$doStep6, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'STEP 6 result')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

