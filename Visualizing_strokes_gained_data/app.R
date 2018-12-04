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
  
  # My app is all about analysing strokes gained by player, round and tournament, so I gave my app an adequate name
  titlePanel("Strokes Gained by Round in Various Tournaments"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("round",
                  "Round Number - Players",
                  choices = x$round,
                  multiple = TRUE,
                  selected = "1"
      ),
      h6("Select the round(s) that total strokes gained for each player will be shown for The Players Championship"),
      
      selectInput("player_last_name",
                  "Player Last Name - Players",
                  choices = x$player_last_name,
                  multiple = TRUE,
                  selected = "Mickelson"
      ),
      
      h6("Select the player(s) for which strokes gained by round data will be shown for The Players Championship"),
      
      selectInput("round2",
                  "Round Number - Memorial",
                  choices = y$round,
                  multiple = TRUE,
                  selected = "1"
      ),
      
      h6("Select the round(s) that total strokes gained for each player will be shown for The Memorial Tournament"),
      
      selectInput("player_last_name2",
                  "Player Last Name - Memorial",
                  choices = y$player_last_name,
                  multiple = TRUE,
                  selected = "Mickelson"),
      
      h6("Select the player(s) for which strokes gained by round data will be shown for The Memorial Tournament"),
      
      selectInput("round3",
                  "Round Number - WGC-Mexico",
                  choices = z$round,
                  multiple = TRUE,
                  selected = "1"
      ),
      
      h6("Select the round(s) that total strokes gained for each player will be shown for WGC-Mexico"),
      
      selectInput("player_last_name3",
                  "Player Last Name - WGC-Mexico",
                  choices = z$player_last_name,
                  multiple = TRUE,
                  selected = "Mickelson"),
      
      h6("Select the player(s) for which strokes gained by round data will be shown for WGC- Mexico")),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", h4("This app is designed to analyze strokes gained data across tournaments, players and rounds. Strokes gained is a statistical metric to determine how well a golfer is doing in relation to another set of golfers shot by shot. 
                                         A positive strokes gained means that the player did better than the field on that shot or in that round. The opposite is true of a negative strokes gained. In this app, the strokes gained represented are in strokes gained 
                                         per round, which is just the total strokes gained per shot in the round all added up. The other three panels represent three tournaments played on the PGA Tour in the 2018 season: The Players Championship, The Memorial Tournament, 
                                         and The World Golf Championship - Mexico. For each graph there are two input selectors, round and player last name. These can be adjusted to compare multiple golfers and multiple rounds within the same tournament. 
                                         By comparing strokes gained across rounds we can predict how well one player did in comparison to another, hypothesizing the order they placed in the tournament. If strokes gained is an accurate measure, the comparison of any number of players should be correct.
                                         
                                         This data came from the PGA Tour Shotlink Intelligence System, which is unfortunately private data. However, more basic data about the PGA Tour can be found at the PGA Tour site here: https://www.pgatour.com/stats.html " )),
                  tabPanel("Players", plotOutput("plot")),
                  tabPanel("Memorial", plotOutput("plot2")),
                  tabPanel("WGC-Mexico", plotOutput("plot3"))
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
      ggplot(aes(x=player_last_name, y=total_strokes_gained, color = round)) +
      geom_point(size = 3) +
      ylab("Total Strokes Gained Per Round") +
      xlab("Player Last Name") +
      ggtitle("Strokes Gained Per Round at the Players Championship")
    
  })
  
  output$plot2 <- renderPlot({
    
    y %>%
      filter( round == input$round2) %>%
      filter( player_last_name == input$player_last_name2) %>%
      select( player_last_name, round,  total_strokes_gained) %>%
      group_by(player_last_name, round) %>%
      ggplot(aes(x=player_last_name, y=total_strokes_gained, color = round)) +
      geom_point(size = 3) +
      ylab("Total Strokes Gained Per Round") +
      xlab("Player Last Name") +
      ggtitle("Strokes Gained Per Round at the Memorial Tournament")
  })
  
  output$plot3 <- renderPlot({
    
    z %>%
      filter( round == input$round3) %>%
      filter( player_last_name == input$player_last_name3) %>%
      select( player_last_name, round,  total_strokes_gained) %>%
      group_by(player_last_name, round) %>%
      ggplot(aes(x=player_last_name, y=total_strokes_gained, color = round)) +
      geom_point(size = 3) +
      ylab("Total Strokes Gained Per Round") +
      xlab("Player Last Name") +
      ggtitle("Strokes Gained Per Round at the WGC-Mexico Tournament")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



