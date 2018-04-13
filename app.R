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
source("lib.r")
#set current working directory
setwd(getwd())

#set input directory
inputDirectory = "True+False Floods"
#all files of the directory
filenames <- list.files(inputDirectory, full.names = TRUE)

#set up writing
logFile = "logFile_listOfTrueFloods.txt"
cat("", file=logFile, sep = "\n")

#setup directory - list of true floods
TrueFloodsDir = 'TrueFloods'
if (dir.exists(TrueFloodsDir)) {
  unlink(TrueFloodsDir, recursive=TRUE)
}
dir.create(TrueFloodsDir)

outputCsvFile = "output.csv"
headerOfoutputCsvFile <- cbind("percentOfUniqueAlarms","natureOfFlood")
write.table( headerOfoutputCsvFile,file=outputCsvFile, sep=',', row.names=F, col.names=F )
tf = 0
ff = 0
#loop over each file
for (f in filenames) {
  c <- calculationsInCsv(f,outputCsvFile)
  o <- labelFlood(f)
  if (o == 'True flood') {
    cat(f, file=logFile, append=TRUE, sep = "\n")
    file.copy(f,TrueFloodsDir)
    tf <- tf + 1
  } else {
    ff <- ff + 1
  }
}

pie_1 = c(tf,ff)
pie_2 = c("true_flood","false_flood")

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
     DT::dataTableOutput("table1"),
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("pc")
     )
   ),
   sidebarLayout(
     DT::dataTableOutput("table"),
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
    
    session$sendCustomMessage(type = 'testmessage',
                              message = 'STEP 1 result')
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

