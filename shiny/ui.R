ui <- fluidPage(
  
  # Application title
  titlePanel("Text prediction!"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(textInput("userInput", "", ""),submitButton("Submit", icon("predict")))
    ),
    
    
    mainPanel(
      fluidRow(textOutput("Prediction"), dataTableOutput("PredictionTable"))
    )
  )
)