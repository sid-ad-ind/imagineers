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
library(data.table)


setwd(getwd())


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Alarm Flood Analysis by Imagineers"),
   tags$head(tags$script(src = "message-handler.js")),
   fileInput("upload", "Upload zip archieve", accept = ".zip"),
   
   actionButton('doStep1', 'Classification'),
   actionButton('doStep2', 'Sequence matrix'),
   actionButton('doStep3', 'Clustering'),
   actionButton('doStep4', 'Repetitive Sequences'),
   
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
       #plotOutput("seqPlot"),
       uiOutput('sequencePlot')
     )
   ),
  
   
   tags$div(class = "result")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$upload, {
    unzip(input$upload$datapath)
  })
  
  observeEvent(input$doStep1, {
    source("classification.r")
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
   
    fullPath <- paste(getwd(),TrueFloodsDir,sep='/')
    fullPath <- paste(fullPath,'/',sep='')
    
    piPath <- paste(getwd(),'sm.py',sep='/')

    files <- list.files(path=fullPath, pattern=".csv", full.name=TRUE)
    
    lists <- list()
    
    for(i in 1:length(files)) {
      data <- read.csv(files[i],stringsAsFactors = FALSE, header= TRUE, row.names = NULL)
      lists[i] <- data['Alarm.Tag']
      
    }
    
    library(reticulate)
    sequence_matrix <- matrix(data=NA, nrow=length(lists), ncol=length(lists))
    source_python(piPath)
    
    for (i in 1:length(lists)) {
      for (j in 1:length(lists)) {
        sequence_matrix[i,j] <- main(lists[[i]],lists[[j]])
      }
    }
    #plot(sequence_matrix)
    output$sequencePlot <- renderTable({
      sequence_matrix
    })
    
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Sequence matrix output')
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
                              message = 'sequence matching pattern')
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

