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
    ## My explanation details how strokes gained are used and how the app can be used to compare players by roudn and by tournament.
    ## The three graphs are showing the same data points but across different tournaments. I wanted to show the method that I used is replicable across tournaments with this data and that players strokes gained can vary across tournaments. FOr example, Mickelson had overall poor strokes gained in the Players and Memorial tournaments so the field was generally better than him, where in the WGC-Mexico he had positive strokes gained in each round and eventually won the tournament.
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Explanation", h4("This app is designed to analyze strokes gained data across tournaments, players and rounds. Strokes gained is a statistical metric to determine how well a golfer is doing in relation to another set of golfers shot by shot. 
                                             A positive strokes gained means that the player did better than the field on that shot or in that round. The opposite is true of a negative strokes gained. In this app, the strokes gained represented are in strokes gained 
                                             per round, which is just the total strokes gained per shot in the round all added up. The other three panels represent three tournaments played on the PGA Tour in the 2018 season: The Players Championship, The Memorial Tournament, 
                                             and The World Golf Championship - Mexico. For each graph there are two input selectors, round and player last name. These can be adjusted to compare multiple golfers and multiple rounds within the same tournament. 
                                             By comparing strokes gained across rounds we can predict how well one player did in comparison to another, hypothesizing the order they placed in the tournament. If strokes gained is an accurate measure, the comparison of any number of players should be correct.
                                             This data came from the PGA Tour Shotlink Intelligence System." )),
                  tabPanel("Players", plotOutput("plot")),
                  tabPanel("Memorial", plotOutput("plot2")),
                  tabPanel("WGC-Mexico", plotOutput("plot3"))
                  )
                  )
      ))

## Define server logic needed to produce the interactive graphs I want to show up in the app. Without this my graphs would not be interactive and the main point of the app would be lost.
server <- function(input, output) {
  
  ## I'm reading in the plot designated for the tab labeled "Players" so that the plot will show up in that tab
  
  output$plot <- renderPlot({
    
    ## Calling the data out of the first RDS file to be shown on the graph so that the inputs to the graph will be reactive
    
    x %>%
      
      ## I'm making sure that the round(s) displayed on the graph is whatever round(s) the input is selected for. This enables the correct round(s) to be shown on the graph for whatever the user is trying to study.
      
      filter( round == input$round) %>%
      
      ## I'm doing the same thing with player_last_name, to make sure only the players that the user wants to see will be shown on the graph.
      
      filter( player_last_name == input$player_last_name) %>%
      
      ## I'm narrowing down the data to only the columns that I want to work with. I wasn't sure exactly how much of the data from the RDS I wanted to use before making the app and this was my way of narrowing that down.
      
      select( player_last_name, round,  total_strokes_gained) %>%
      
      ## I'm making sure that the two variables I'm manipulating in the sidepanel a spread enough in order to function properly
      
      group_by(player_last_name, round) %>%
      
      ## I'm creating my plot to visualize the data that I have selected and filtered. At first I had the player_last_name on the y-axis and the total_strokes_gained on the x-axis but then by friend Ollie Cordeiro told me it would be better if I switched them because total_strokes_gained is really the variable the user wants to look at. I agreed with him and decided to color by round because it's easy to distingush between the four shade of blue.
      
      ggplot(aes(x=player_last_name, y=total_strokes_gained, color = round)) +
      
      ## I decided points would be the best way to visualize the intersection between player and total strokes gained and made those points size three in order to make sure that they are visible but not dominting the graph if  many players are compared at once.
      
      geom_point(size = 3) +
      
      ## I labeled the y-axis an appropriate name for what was displayed by it
      
      ylab("Total Strokes Gained Per Round") +
      
      ## I labeled the x-axis an approapriate name for what was displayed by it
      
      xlab("Player Last Name") +
      
      ## I labeled the title more specfically than the title panel header because this is the graph specifically for The Players Champioinship
      
      ggtitle("Strokes Gained by Player Per Round at the Players Championship")
    
  })
  
  ## I'm reading in the plot designated for the tab labeled "Memorial" so that the plot will show up in that tab
  
  output$plot2 <- renderPlot({
    
    ## Calling the data out of the second RDS file to be shown on the graph so that the inputs to the graph will be reactive
    
    y %>%
      
      ## I'm making sure that the round(s) displayed on the graph is whatever round(s) the input is selected for. This enables the correct round(s) to be shown on the graph for whatever the user is trying to study.
      
      filter( round == input$round2) %>%
      
      ## I'm doing the same thing with player_last_name, to make sure only the players that the user wants to see will be shown on the graph.
      
      filter( player_last_name == input$player_last_name2) %>%
      
      ## I'm narrowing down the data to only the columns that I want to work with. I wasn't sure exactly how much of the data from the RDS I wanted to use before making the app and this was my way of narrowing that down.
      
      select( player_last_name, round,  total_strokes_gained) %>%
      
      ## I'm making sure that the two variables I'm manipulating in the sidepanel a spread enough in order to function properly
      
      group_by(player_last_name, round) %>%
      
      ## I'm creating my plot to visualize the data that I have selected and filtered. At first I had the player_last_name on the y-axis and the total_strokes_gained on the x-axis but then by friend Ollie Cordeiro told me it would be better if I switched them because total_strokes_gained is really the variable the user wants to look at. I agreed with him and decided to color by round because it's easy to distingush between the four shade of blue.
      
      ggplot(aes(x=player_last_name, y=total_strokes_gained, color = round)) +
      
      ## I decided points would be the best way to visualize the intersection between player and total strokes gained and made those points size three in order to make sure that they are visible but not dominting the graph if  many players are compared at once.
      
      geom_point(size = 3) +
      
      ## I labeled the y-axis an appropriate name for what was displayed by it
      
      ylab("Total Strokes Gained Per Round") +
      
      ## I labeled the x-axis an approapriate name for what was displayed by it
      
      xlab("Player Last Name") +
      
      ## I labeled the title more specfically than the title panel header because this is the graph specifically for The Memorial Tournament
      
      ggtitle("Strokes Gained by Player Per Round at the Memorial Tournament")
  })
  
  ## I'm reading in the plot designated for the tab labeled "WGC-Mexico" so that the plot will show up in that tab
  
  output$plot3 <- renderPlot({
    
    ## Calling the data out of the third RDS file to be shown on the graph so that the inputs to the graph will be reactive
    
    z %>%
      
      ## I'm making sure that the round(s) displayed on the graph is whatever round(s) the input is selected for. This enables the correct round(s) to be shown on the graph for whatever the user is trying to study.
      
      filter( round == input$round3) %>%
      
      ## I'm doing the same thing with player_last_name, to make sure only the players that the user wants to see will be shown on the graph.
      
      filter( player_last_name == input$player_last_name3) %>%
      
      ## I'm narrowing down the data to only the columns that I want to work with. I wasn't sure exactly how much of the data from the RDS I wanted to use before making the app and this was my way of narrowing that down.
      
      select( player_last_name, round,  total_strokes_gained) %>%
      
      ## I'm making sure that the two variables I'm manipulating in the sidepanel a spread enough in order to function properly
      
      group_by(player_last_name, round) %>%
      
      ## I'm creating my plot to visualize the data that I have selected and filtered. At first I had the player_last_name on the y-axis and the total_strokes_gained on the x-axis but then by friend Ollie Cordeiro told me it would be better if I switched them because total_strokes_gained is really the variable the user wants to look at. I agreed with him and decided to color by round because it's easy to distingush between the four shade of blue.
      
      ggplot(aes(x=player_last_name, y=total_strokes_gained, color = round)) +
      
      ## I decided points would be the best way to visualize the intersection between player and total strokes gained and made those points size three in order to make sure that they are visible but not dominting the graph if  many players are compared at once.
      
      geom_point(size = 3) +
      
      ## I labeled the y-axis an appropriate name for what was displayed by it
      
      ylab("Total Strokes Gained Per Round") +
      
      ## I labeled the x-axis an approapriate name for what was displayed by it
      
      xlab("Player Last Name") +
      
      ## I labeled the title more specfically than the title panel header because this is the graph specifically for The WGC-Mexico Tournament
      
      ggtitle("Strokes Gained by Player Per Round at the WGC-Mexico Tournament")
  })
}

## Running the app so that it functions in an html space

shinyApp(ui = ui, server = server)






