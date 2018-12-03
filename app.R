#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(janitor)
library(readxl)
library(tidyverse)

x <- read_rds("finalproject.rds")
y <- read_rds("finalproject2.0.rds")
z <- read_rds("finalproject3.0.rds")

## Reading in my libraries and the RDS with all of the data that I want to use in it

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Strokes Gained by Round in The Players Championship"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("round",
                     "Round Number - Players",
                     choices = x$round,
                     multiple = TRUE,
                     selected = "1"
      ),
      selectInput("player_last_name",
                  "Player Last Name - Players",
                  choices = x$player_last_name,
                  multiple = TRUE,
                  selected = "Mickelson"
      ),
   
      selectInput("round",
                  "Round Number - Memorial",
                  choices = y$round,
                  multiple = TRUE,
                  selected = "1"
      ),
      selectInput("player_last_name",
                  "Player Last Name - Memorial",
                  choices = y$player_last_name,
                  multiple = TRUE,
                  selected = "Mickelson"),
      
      selectInput("round",
                  "Round Number - WGC-Mexico",
                  choices = z$round,
                  multiple = TRUE,
                  selected = "1"
      ),
      selectInput("player_last_name",
                  "Player Last Name - WGC-Mexico",
                  choices = z$player_last_name,
                  multiple = TRUE,
                  selected = "Mickelson")),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Players", plotOutput("plot")),
                    tabPanel("Memorial", plotOutput("plott")),
                    tabPanel("WGC-Mexico", plotOutput("plottt"))
      )
   )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
     
     x %>%
       filter( round == input$round) %>%
       filter( player_last_name == input$player_last_name) %>%
       select( player_last_name, round,  total_strokes_gained) %>%
       group_by(player_last_name, round) %>%
       ggplot(aes(x=total_strokes_gained, y=player_last_name, color = round)) +
       geom_point(size = 3) +
       xlab("Total Strokes Gained Per Round") +
       ylab("Player Last Name") +
       ggtitle("Strokes Gained Per Round at the Players Championship")
   
      })
   
   output$plott <- renderPlot({
     
     y %>%
       filter( round == input$round) %>%
       filter( player_last_name == input$player_last_name) %>%
       select( player_last_name, round,  total_strokes_gained) %>%
       group_by(player_last_name, round) %>%
       ggplot(aes(x=total_strokes_gained, y=player_last_name, color = round)) +
       geom_point(size = 3) +
       xlab("Total Strokes Gained Per Round") +
       ylab("Player Last Name") +
       ggtitle("Strokes Gained Per Round at the Memorial Tournament")
   })
   
   output$plottt <- renderPlot({
     
     z %>%
       filter( round == input$round) %>%
       filter( player_last_name == input$player_last_name) %>%
       select( player_last_name, round,  total_strokes_gained) %>%
       group_by(player_last_name, round) %>%
       ggplot(aes(x=total_strokes_gained, y=player_last_name, color = round)) +
       geom_point(size = 3) +
       xlab("Total Strokes Gained Per Round") +
       ylab("Player Last Name") +
       ggtitle("Strokes Gained Per Round at the WGC-Mexico Tournament")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

