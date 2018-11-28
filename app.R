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
      selectInput("player",
                  "Player Last Name",
                  choices = x$player_last_name,
                  multiple = TRUE,
                  selected = "Woods")),
   
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     x %>%
       #filter( player_last_name == input$player_last_name) %>%
       filter( round == input$round) %>%
       select(player_first_name, player_last_name, round, hole, strokes_gained_baseline, player_number) %>%
       group_by(player_last_name, round) %>%
       filter(player_number %in% c(29221,26331,25632,48081,28089,25686,33141,37189,30911,29926,36689,33448,34563,24502,24138,8793,27349,29461,21961,26499)) %>%
       mutate(total_strokes_gained = sum(strokes_gained_baseline)) %>%
       ggplot(aes(x=total_strokes_gained, y=player_last_name, color = )) +
       geom_point() +
       xlab("Total Strokes Gained") +
       ylab("Player Last Name") +
       ggtitle("Strokes Gained Per Round of the Top 20 at the Players Championship")
   
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

