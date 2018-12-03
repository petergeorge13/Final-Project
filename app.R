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

## Reading in my libraries and the RDS with all of the data that I want to use in it

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Strokes Gained by Round in The Players Championship"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("round",
                     "Round Number",
                     choices = x$round,
                     multiple = TRUE,
                     selected = "1"
      ),
      selectInput("player_last_name",
                  "Player Last Name",
                  choices = x$player_last_name,
                  multiple = TRUE,
                  selected = "Woods")),
   
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Players", plotOutput("plot"))
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
       #filter(player_number %in% c(29221,26331,25632,48081,28089,25686,33141,37189,30911,29926,36689,33448,34563,24502,24138,8793,27349,29461,21961,26499)) %>%
       #mutate(total_strokes_gained = sum(strokes_gained_baseline)) %>%
       ggplot(aes(x=total_strokes_gained, y=player_last_name, color = round)) +
       geom_point(size = 3) +
       xlab("Total Strokes Gained") +
       ylab("Player Last Name") +
       ggtitle("Strokes Gained Per Round at the Players Championship")
   
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

