## Reading in my libraries so that R will be capable to perform all of the function I want to run in this shiny app

library(shiny)
library(janitor)
library(readxl)
library(tidyverse)

## I'm reading in all three RDS files that I created in my Rmd in order to have a complete sets of modified data from all three tournaments that I can work with.

x <- read_rds("finalproject.rds")
y <- read_rds("finalproject2.0.rds")
z <- read_rds("finalproject3.0.rds")


## I'm defining UI for my app that will end up creating my four tabs, three interactive graphs and all of the texts inside the app. WIthout this the app wouldn't run.

ui <- fluidPage(
  
  ## My app is all about analysing strokes gained by player, round and tournament, so I gave my app an adequate name in the title panel which will be shown above all of my tabs.
  
  titlePanel("Strokes Gained by Round and Player in Various Tournaments"),
  
  ## Creating a sidebar with one panel icluding six input dropdowns and six segments of text. I wanted to make sure that each graph would be interactive and that there was enough explination for each of them for anyone in the future to be able to come in and use the app.
  ## I'm creating multiple inputs for both rounds and players because I want the user to be able to select over what rounds and which players they want the total strokes gained data to show up for.
  ## This will allow the user to compare multiple players over multiple rounds to see who performed better or worse by round
  ## I left the defaults as round one and Mickelson because tournaments start with the first round and most people recognize the name Phil Mickelson more than most other professional golfers, other than Tiger Woods. Unforunately Tiger did not play in the WGC-Mexico tournament and I wanted all of the defaults to be the same.
  
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
    
    ## I created a main panel with four different tabs, the first being an explination of what is going on in the app and than the next three being interactive graphs for three different tournaments that can be adjusted by using the sidebars corresponsing to that tournament.
    ##
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Explanation", h4("This app is designed to analyze strokes gained data across tournaments, players and rounds. Strokes gained is a statistical metric to determine how well a golfer is doing in relation to another set of golfers shot by shot. 
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






