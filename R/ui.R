###############################################################################
# THIS IS THE BEGINNINGS OF AN APP SIMILAR TO AMERICAN SOCCER ANALYSIS TO 
# ANALYZE SOCCER DATA, FOCUSED FIRST ON THE NWSL.
###############################################################################
library(timevis)
library(visNetwork)
library(tidyverse)
library(shiny)
source('data_loading.R')

## Define UI for dataset viewer application -----------------------------------
fluidPage(
  
  ## App title -----------------------------------------------------------------
  titlePanel("Soccer Analysis"), # Need a new title
  
  ## Sidebar layout with input and output definitions --------------------------
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      h2('Team Selection'),
      
      # Could have a season selection
      # selectInput(inputId = "season_selection",
      #             label = "Season",
      #             choices = team_name_list),
      
      # Input: Primary Team name selection
      selectInput(inputId = "team_name",
                label = "Team Name",
                choices = c('',team_name_list)),
      
      # Opposing team selection
      uiOutput('away_team_selection'),
      
      # Date of game
      uiOutput('game_dates'),
      
      # This should be separated out and be dependent on the tab selection
      # # Input: Text for first name
      # textInput(inputId = "first_name",
      #           label = "First Name",
      #           value = ""),
      # 
      # # Input: Text for last name
      # textInput(inputId = "last_name",
      #             label = "Last Name",
      #             value = ""),
      
      br()
      
    ),
    
    ## Main panel that shows captions and determines outputs -------------------
    mainPanel(
      # h3(textOutput("Visual_Title")), 
      # 
      # h5(textOutput("Visual_Test")),
      
      # Output: Tabset w/ four tabs -------------------------------------------
      tabsetPanel(type = "tabs",
                  # tabPanel("Introduction", 
                  #          htmlOutput("intro_text")),
                  tabPanel("Team Formation", 
                           plotOutput("field_w_formation")),
                  tabPanel("Player Stats", 
                           tableOutput('test')),
                  tabPanel("Player Visuals", 
                           visNetworkOutput("passing_network", height = '750px'), 
                           downloadButton('download_network', 'export as html'))
      )
      
    )
  )
)
