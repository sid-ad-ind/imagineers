#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Alarm Flood Analysis by Imagineers"),
   tags$head(tags$script(src = "message-handler.js")),
   actionButton('doStep1', 'STEP 1'),
   actionButton('doStep2', 'STEP 2'),
   actionButton('doStep3', 'STEP 3'),
   actionButton('doStep4', 'STEP 4'),
   actionButton('doStep5', 'STEP 5'),
   actionButton('doStep6', 'STEP 6'),
   
   tags$div(class = "result")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  observeEvent(input$doStep1, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'STEP 1 result')
  })
  observeEvent(input$doStep2, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'STEP 2 result')
  })
  observeEvent(input$doStep3, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'STEP 3 result')
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

