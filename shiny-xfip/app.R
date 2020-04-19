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

here::here(        ## Does the app SOURCE
  'WORKAFTERCORONA', 'future-predictions.R'
) %>% 
  source()


ui <- navbarPage("",
        tabPanel("Home",
          fluidPage(
            titlePanel("Capstone Project"),
            mainPanel(
              h3("Introduction"),
              p("This shiny app is a University of Virginia capstone project in statistics, created by second-year 
                students Devan Bose, Jordan Denish, Malcolm Mashig, and Christian Rogers. We created a model to predict 
                the future xFIP value for MLB starting pitchers. Our results are contained in this app."),
              h3("Definition of xFIP"),
              p("Expected Fielding Independent Pitching (xFIP) is an advanced baseball statistic that is an 
                estimate for a pitcher’s earned run average (ERA). A pitcher’s main objective is to prevent runs and 
                xFIP is considered to be a better predictor of future performance than ERA, which can mask the actual 
                skill of a pitcher. xFIP is a common estimate for a pitcher’s ERA that only factors in the batter 
                outcomes that a pitcher can control (strikeouts, walks, and hit by pitches) while also normalizing 
                for home runs allowed by using the league average home run/fly ball rate. xFIP is calculated using 
                the formula xFIP = ((13*(Fly balls * lgHR/FB%))+(3*(BB+HBP))-(2*K))/IP + constant. The constant is 
                generally around 3.10, which helps adjust the xFIP value to an ERA scale."),
              h3("Features of the App"),
              p("- Click on an individual player in the dropdown menu to view their future projections in table and 
              graph form"),
              p("- Click on a team from the dropdown menu to subset the data to players from your favorite team"),
              p("- Click on one of the column headers below to sort the entire dataset by that statistic")
            ))),
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
        tabPanel("Calculator",
          fluidPage(
            titlePanel("xFIP Prediction Calculator"),
            numericInput("age", "Current Age", 0),
            numericInput("lagxfip3", "xFIP - 3 Seasons Ago", 0),
            numericInput("lagxfip2", "xFIP - 2 Season Ago", 0),
            numericInput("lagxfip", "Current Season xFIP", 0),
            numericInput("fbv", "Current Season FBv", 0),
            numericInput("fbp", "Current Season FBp", 0),
            tableOutput("calculatedVal")
          )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$calculatedVal <- renderTable({
    calcFuture <- data_frame(lag_xfip = NA, lag_fbv = NA, lag_fbp = NA, lag_age = NA, lag_xfip2 = NA, lag_xfip3 = NA)
    calcFuture <- calcFuture%>%
      mutate(   #### NEED TO STANDARDIZE THE INPUTS FOR THE FUNCTION
        lag_age = input$age,
        lag_xfip3 = input$lagxfip3,
        lag_xfip2 = input$lagxfip2,
        lag_xfip = input$lagxfip,
        lag_fbv = input$fbv,
        lag_fbp = input$fbp,
        age_range = case_when(
          lag_age < 28 ~ "young",
          # between(lag_age, 28, 30) ~ "prime",
          lag_age >= 28 ~ "old"
        )
      )
    calcFuture <- calcFuture %>% 
      mutate(
        predicted_fbv1 = fbv_submodel %>% predict(newdata = calcFuture),
        predicted_fbp1 = fbp_submodel %>% predict(newdata = calcFuture)
      )
    
    calcFuture1 <- calcFuture %>% 
      filter(is.na(lag_xfip2))
    
    calcFuture1 <- calcFuture1 %>% 
      mutate(
        predicted_xfip1 = xfip_model %>% predict(newdata = calcFuture1)
      )
    
    calcFuture2 <- calcFuture %>% 
      filter(!is.na(lag_xfip2))
    
    calcFuture2 <- calcFuture2 %>% 
      mutate(
        predicted_xfip1 = xfip_model2 %>% predict(newdata = calcFuture2)
      )
    
    calcFuture <- bind_rows(calcFuture1, calcFuture2)
    
    # Second Prediction ------------------------
    
    calcFuture <- calcFuture %>% 
      mutate(
        lag_xfip2 = lag_xfip,
        lag_xfip = predicted_xfip1,
        lag_fbv = predicted_fbv1,
        lag_fbp = predicted_fbp1,
        lag_age = lag_age + 1,
        age_range = case_when(
          lag_age < 28 ~ "young",
          # between(lag_age, 28, 30) ~ "prime",
          lag_age >= 28 ~ "old"
        )
      )
    
    calcFuture <- calcFuture %>% 
      mutate(
        predicted_fbv2 = fbv_submodel %>% predict(newdata = calcFuture),
        predicted_fbp2 = fbp_submodel %>% predict(newdata = calcFuture)
      )
    
    calcFuture <- calcFuture %>% 
      mutate(
        predicted_xfip2 = xfip_model2 %>% predict(newdata = calcFuture)
      )
    
    # Third Prediction ------------------------
    
    calcFuture <- calcFuture %>% 
      mutate(
        lag_xfip3 = lag_xfip2,
        lag_xfip2 = lag_xfip,
        lag_xfip = predicted_xfip2,
        lag_fbv = predicted_fbv2,
        lag_fbp = predicted_fbp2,
        lag_age = lag_age + 1,
        age_range = case_when(
          lag_age < 28 ~ "young",
          # between(lag_age, 28, 30) ~ "prime",
          lag_age >= 28 ~ "old"
        )
      )
    
    calcFuture <- calcFuture %>% 
      mutate(
        predicted_fbv3 = fbv_submodel %>% predict(newdata = calcFuture),
        predicted_fbp3 = fbp_submodel %>% predict(newdata = calcFuture)
      )
    
    calcFuture <- calcFuture %>% 
      mutate(
        predicted_xfip3 = xfip_model3 %>% predict(newdata = calcFuture)
      )
    
    calcFuturePredictions <- calcFuture %>% 
      select(
        predicted_xfip1, predicted_xfip2, 
        predicted_xfip3
      )
    
    #####  NOW UNSTANDARDIZE
# 
#     stdzFuture <- fangraphs_clean %>% 
#       group_by(Season) %>% 
#       summarise(
#         sd_xfip = sqrt((var(xFIP, na.rm = TRUE) + var(lag_xfip, na.rm = TRUE) +
#                           var(lag_xfip2, na.rm = TRUE)) / 3),
#         mean_xfip = (mean(xFIP, na.rm = TRUE) + mean(lag_xfip, na.rm = TRUE) +
#                        mean(lag_xfip2, na.rm = TRUE)) / 3
#         # sd_xfip = sd(xFIP),
#         # mean_xfip = mean(xFIP)
#       ) 
#     calcFuturePredictions <- calcFuturePredictions%>%
#       mutate(
#         # xFIP = xFIP * sd_xfip + mean_xfip, # NOT REAL de-standardized
#         predicted_xfip1 = round(predicted_xfip1[1] * stdzFuture[36, 2] + stdzFuture[36, 3], 2),
#         predicted_xfip2 = round(predicted_xfip2[1] * stdzFuture[36, 2] + stdzFuture[36, 3], 2),
#         predicted_xfip3 = round(predicted_xfip3[1] * stdzFuture[36, 2] + stdzFuture[36, 3], 2)
#       ) 
    calcFuturePredictions
    
  })
  
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
        geom_text(aes(2020,leagueAverageXFIP, hjust = 2, vjust = -1, label = "League Average XFIP", color = "red")) +
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

