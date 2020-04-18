#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(here)

here::here(
  'WORKAFTERCORONA', 'future-predictions.R'
) %>% 
  source()

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Basic DataTable"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("sp",
                       "Starting Pitcher:",
                         c("All", unique(as.character(predictions$Name))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    if (input$sp == "All") {
      predictions
    } else {
      predictions[predictions$Name == input$sp,]
    }
  }))
  
}

# Run the application 
shinyApp(ui = ui, server = server)

