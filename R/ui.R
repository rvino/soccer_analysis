###############################################################################
# THIS IS THE PROPERTY OF THE SAN FRANCISCO DISTRICT ATTORNEY'S
# OFFICE AND ALL CODE HEREIN IS ACCREDITED TO THAT OFFICE.
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
      
      # Input: Text for first name
      textInput(inputId = "first_name",
                label = "First Name",
                value = ""),
      
      # Input: Text for last name
      textInput(inputId = "last_name",
                  label = "Last Name",
                  value = ""),
      
      br(),
      
      selectInput(inputId = "centrality", 
                  label = "Choose a centrality measure:", 
                  choices = c("None", 
                              "Betweenness", 
                              "Closeness", 
                              "Degree",
                              "Eigen")),
      
      numericInput(inputId = "degree_separation", 
                   label = "Degrees from suspect to view:", 
                   value = 1,
                   min = 1,
                   max = 5)
      
    ),
    
    ## Main panel that shows captions and determines outputs -------------------
    mainPanel(
      h3(textOutput("Network_Title")), 
      
      h5(textOutput("Network_Test")),
      
      # Output: Tabset w/ four tabs -------------------------------------------
      tabsetPanel(type = "tabs",
                  # tabPanel("Introduction", 
                  #          htmlOutput("intro_text")),
                  tabPanel("Team Formation", 
                           plotOutput("field_w_formation")),
                  tabPanel("Player Stats", 
                           timevisOutput('timeline'),
                           leaflet:::leafletOutput("sfmap", height=600, width=800),
                           tableOutput('test')),
                  tabPanel("Player Visuals", 
                           visNetworkOutput("network_suspect", height = '750px'), 
                           downloadButton('download_network', 'export as html'))
      )
      
    )
  )
)
