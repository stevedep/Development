ui <- fluidPage(
  
  
  sidebarLayout(
    sidebarPanel( "This is a text prediction algorithm using back off method to ensure results.
                  The application has been created by Steve de Peijper, 2016", wwidth = 1),
    mainPanel(
      titlePanel("Text prediction!"),
      fluidRow(column(6,textInput("userInput", "Input text here", "this is an"))),
      fluidRow(column(6,submitButton("Submit", icon("predict")))),
      br(),h1(fluidRow(column(6,textOutput("Prediction")))), br(),
      fluidRow(column(6,dataTableOutput("PredictionTable")))
    )
  )
)
 