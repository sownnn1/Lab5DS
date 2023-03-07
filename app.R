# Load required libraries
library(shiny)

# Load the iris dataset
iris <- read.csv("Iris.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("Shiny Text"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dataset",
        label = "Choose a dataset to view:",
        choices = c("Iris" = "iris", "Rock Types" = "rock", "Atmospheric Pressure" = "pressure", "Car Speeds" = "cars")
      ),
      numericInput(
        inputId = "obs",
        label = "Number of observations to view:",
        value = 10
      ),
      downloadButton("downloadData", "Download Selected Dataset"),
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv", "text/comma-seperated-values, text/plain", ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      tags$hr(),
      radioButtons(
        "disp",
        "Display",
        choices = c(Head = "head", All = "all"),
        selected = "head"
      ),
      tags$hr(),
      p("Current time:"),
      textOutput("time")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Table", tableOutput("view")),
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Create a reactive timer with interval 1000 milliseconds
  autoInvalidate <- reactiveTimer(1000)
  
  # Display the current time
  output$time <- renderText({
    paste("The current time is:", Sys.time())
  })
  
  datasetInput <- reactive({
    switch (
      input$dataset,
      "rock" = rock,
      "pressure" = pressure,
      "cars" = cars,
      "iris" = iris
    )
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  # Trigger the reactive timer every second to update the current time
  observe({
    autoInvalidate()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
