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


ui <- navbarPage("Capstone Project",
        tabPanel("Home"),
        tabPanel("Predictions",
          fluidPage(
            titlePanel("Three Year xFIP Predictions"),
            fluidRow(
              column(4,
                  selectInput("sp",
                              "Starting Pitcher:",
                          c("All", unique(as.character(sort(predictions$Name)))))),
              column(4,
                  selectInput("team",
                              "Team:",
                          c("All", unique(as.character(sort(predictions$Team))))))
                      ),
            htmlOutput("picture"),
            DT::dataTableOutput("table"),
            tableOutput("stats"),
            plotOutput("plot"))),
        tabPanel("Calculator"))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$picture <- renderText({
    if (input$sp != "All") {
      src <- predictionsURLS %>% 
        filter(Name == input$sp) %>% 
        select(URL) %>% 
        as_vector()
      c('<img src="',src,'" align="middle" width = 106.5 height = 160>')
    }
    })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    if (input$sp == "All" & input$team == "All") {
      predictions
    } else if (input$sp != "All") {
      predictions[predictions$Name == input$sp,]
    } else {
      predictions[predictions$Team == input$team,]
    }
  }))
  
  output$stats <- renderTable({
    if (input$sp == "All" & input$team == "All") {
    } else if (input$sp != "All") {
      fangraphs_clean %>% 
        filter(Name == input$sp) %>% 
        mutate(Season = as.integer(Season),
               Age = as.integer(Age),
               IP = as.integer(IP)) %>% 
        select(
          Season, Team, 
          Age, IP, ERA, WHIP, 
          FIP, xFIP, `K/BB`, WAR
        )
    } else {
    }
  })
  
  output$plot <- renderPlot({
    if (input$sp != "All") {
      p <- predictions %>% 
        filter(Name == input$sp)
      t <- tibble(year = 2018:2022,
             xFIP = c(p$`2018 xFIP`, p$`2019 xFIP`, 
                      p$`2020 xFIP (Predicted)`, p$`2021 xFIP (Predicted)`,
                      p$`2022 xFIP (Predicted)`))
      t %>% 
        ggplot() +
        geom_line(aes(year, xFIP, linetype = year > 2019), size = 2.5) +
        geom_hline(aes(yintercept=leagueAverageXFIP, color = "red")) +
        geom_text(aes(2020,leagueAverageXFIP, vjust = -1, label = "League Average XFIP", color = "red")) +
        geom_point(aes(year, xFIP), size = 5, color = "maroon") +
        theme_linedraw() + 
        theme(legend.position = "none",
              axis.title.x = element_text(size = 30, face = "bold"),
              axis.text.x = element_text(size = 22),
              axis.title.y = element_text(size = 30, face = "bold"),
              axis.text.y = element_text(size = 22)) +
        ylim(2, 7)
    } else if (input$sp == "All" & input$team == "All") {} else {
      p <- predictions %>%
        filter(Team == input$team)
      t <- p %>%
        pivot_longer(contains("xFIP"), names_to = "year", values_to = 'xFIP') %>%
        mutate(year = as.numeric(str_sub(year, 1, 4)))
      t %>%
        ggplot() +
        theme_linedraw() +
        geom_line(aes(year, xFIP, color = Name, linetype = year > 2019), size = 2.5,
                  show.legend = FALSE) +
        geom_hline(aes(yintercept=leagueAverageXFIP)) +
        geom_text(aes(2020,leagueAverageXFIP, vjust = -1, label = "League Average XFIP")) +
        ## Maybe change color to red here
        geom_point(aes(year, xFIP, color = Name), size = 5) +
        theme(axis.title.x = element_text(size = 30, face = "bold"),
              axis.text.x = element_text(size = 22),
              axis.title.y = element_text(angle = 0, size = 30, face = "bold"),
              axis.text.y = element_text(size = 22),
              legend.title = element_text(size = 25, face = "bold"),
              legend.text = element_text(size = 20)) +
        ylim(2, 7)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

