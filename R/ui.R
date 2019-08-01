###############################################################################
# THIS IS THE BEGINNINGS OF AN APP SIMILAR TO AMERICAN SOCCER ANALYSIS TO 
# ANALYZE SOCCER DATA, FOCUSED FIRST ON THE NWSL.
###############################################################################
library(timevis)
library(visNetwork)
library(tidyverse)
library(shiny)
source('StatsbombData_v1.R')

## Define UI for dataset viewer application -----------------------------------
fluidPage(
  
  ## App title -----------------------------------------------------------------
  titlePanel("Soccer Analysis"), # Need a new title
  
  ## Sidebar layout with input and output definitions --------------------------
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      h2('Player Information'),
      
      # Input: Team name selection
      selectInput(inputId = "team_name",
                label = "Team Name",
                choices = team_name_list),
      
      selectInput(inputId = "opp_team_name",
                  label = "Opposing Team Name",
                  choices = team_name_list),
      
      # This stuff isn't necessary for right now until player specific stats are ready
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
