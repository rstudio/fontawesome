library(shiny)
library(DT)
library(ggplot2)
library(fontawesome)

ui <- fluidPage(

  titlePanel("Basic DataTable"),

  # Create a new Row in the UI for selectInputs
  fluidRow(

    column(
      width = 4,
      selectInput(
        inputId = "man",
        label = tags$p(fa("car", fill = "purple"), "Manufacturer:"),
        choices = c(
          "All",
          unique(as.character(mpg$manufacturer))))
    ),

    column(
      width = 4,
      selectInput(
        inputId = "trans",
        label = tags$p(fa("car", fill = "forestgreen"), "Transmission:"),
        choices = c(
          "All",
          unique(as.character(mpg$trans))))
    ),

    column(
      width = 4,
      selectInput(
        inputId = "cyl",
        label = tags$p(fa("car", fill = "steelblue"), "Cylinders:"),
        choices = c(
          "All",
          unique(as.character(mpg$cyl))))
    )
  ),

  # Create a new row for the table.
  fluidRow(
    dataTableOutput("table")
  )
)

server <- function(input, output) {

  # Filter data based on selections
  output$table <- renderDataTable({

    data <- mpg
    if (input$man != "All") {
      data <- data[data$manufacturer == input$man,]
    }
    if (input$cyl != "All") {
      data <- data[data$cyl == input$cyl,]
    }
    if (input$trans != "All") {
      data <- data[data$trans == input$trans,]
    }
    data
  })
}

shinyApp(ui = ui, server = server)
