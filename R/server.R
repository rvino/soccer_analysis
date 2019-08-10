###############################################################################
#########                    SHINY SERVER SCRIPT                     ##########
###############################################################################
# This script is the servever side of the Shiny app 

## Loading Packages -----------------------------------------------------------
library(visNetwork)
library(htmlwidgets)
library(tidyverse)
library(shiny)
library(shinythemes)
library(readtext)

## Loading in data ------------------------------------------------------------
# Need to load: events_data
# Loading in source
# introduction <- readtext('Introduction Copy.docx')
source('data_loading.R')

## Define server logic to summarize and view data -----------------------------
shinyServer(function(input, output, session) {
  
  ## Load instructions --------------------------------------------------------
  # output$intro_text <- renderUI({HTML(paste(introduction$text))})
  
  ## Test for if name appears ----------------------------------
  # central_node <- reactive({
  #   # Ensuring that the input$last_name is upper case
  #   test_name <- paste0()
  #   
  #   # Calculate nearest strings
  #   distance = levenshteinSim(test_name, df_events$player.name)
  #   
  #   if (test_name %in% df_events$player.name){
  #     name_used <- test_name 
  #   }
  #   else if (max(distance) > 0.7){
  #     # Taking the best matched string given the user entry
  #     name_used <- df_events$player.name[distance == max(distance)][1]
  #   }
  #   else {
  #     name_used <- test_name
  #   }
  #   return(name_used)
  # })
  
  # The output$Visual_Title is computed based on a reactive expression that
  # returns df_events$player.name. When the user changes first name, last name, or DOB:
  #
  #  1) This expression is automatically called to recompute the output 
  #  2) The new caption is pushed back to the browser for re-display
  # 
  # output$Visual_Title <- renderText({
  #   paste0(str_to_title(input$first_name), ' ', str_to_title(input$last_name), "'s stats")
  # })
  
  # output$Visual_Test <- renderText({
  #   if (central_node() %in% df_events$player.name){
  #     paste0(str_to_title(input$first_name), ' ', str_to_title(input$last_name), " appears in the data. This is the name closest to what you
  #            entered.")
  #   }
  #   else{
  #     paste0(str_to_title(input$first_name), ' ', str_to_title(input$last_name), " does not appear in the data. Be sure that you've entered first name, last name, and date of birth in the form of YYYY-MM-DD.")
  #   }
  #   })
  
  ## Limiting opponent team list and game dates by selections -----------------
  output$away_team_selection <- renderUI({
    selectInput(inputId = "opp_team_name",
                label = "Opposing Team Name",
                choices = c('', team_name_list[team_name_list!=input$team_name]))
  })
  
  output$game_dates <- renderUI({
    selectInput(inputId = "game_date",
                label = "Game Date",
                choices = c('',df_lineups$match_date[df_lineups$team.name==input$team_name &
                                                  df_lineups$away_team.away_team_name==input$opp_team_name]))
  })
  
  ## Drawing field with formation ---------------------------------------------
  output$field_w_formation <- renderPlot({
    
    # Getting the formation data
    formation.dataframe <- df_lineups %>%
      filter(team.name==input$team_name & lineup.rank==1) 
    
    primary_color <- formation.dataframe$primary_color[1]
    secondary_color <- formation.dataframe$secondary_color[1]
    
    # plotting formation
    formation_field <- ggplot(data=formation.dataframe,
                         aes(x=x, y=y)) +
      draw_pitch(pitch_lines = 'white', pitch_color = '#006d2c')+
      geom_point(color=primary_color, size=8)+
      geom_point(shape = 1,size = 9,colour = secondary_color)+
      geom_text(aes(label=jersey_number), color='white')+
      #coord_flip()+
      pitch_theme

    plot(formation_field)
  })
  
  output$shot_field <- renderPlot({
    
    shot_field <- ggplot(data=filter(shots_sample_data, team.name==input$team_name),
                         aes(x=location.x, y=location.y)) +
      draw_pitch(pitch_lines = 'white', pitch_color = 'green')+
      geom_point(color='blue')+
      #coord_flip()+
      pitch_theme
    
    plot(shot_field)
  })
  #############################################################################
  ##########                  Sample Table                           ##########
  #############################################################################
  # output$sample_table <- renderDT({
  #   datatable(crime_hist %>%
  #     filter(UID == central_node()),
  #     colnames = c('Date', 'Event', 'ID', 'Role', 'Description', 'Category'),
  #     extensions = c('Buttons'), options = list(
  #       dom = 'Brtip',
  #       buttons =
  #         list('colvis', 'copy', 'print', list(
  #           extend = 'collection',
  #           buttons = c('csv', 'excel', 'pdf'),
  #           text = 'Download'))
  #     )) %>%
  #     formatDate(1, method = 'toLocaleString')
  # })


  # #############################################################################
  # ##########                   TIMELINE                              ##########
  # #############################################################################
  # timeline_info <- reactive({
  #   timeline_data <- all_data_clean %>%
  #     filter(UID == central_node()) %>%
  #     mutate(DATE = as.Date(DATE)) %>%
  #     rename(start = DATE) %>%
  #     rename(content = INCIDENT_NO)
  # })
  # 
  # output$timeline <- renderTimevis({
  #   timevis(timeline_info())
  # })
  
  
  #############################################################################
  ##########                Graphing Network                 ##########
  #############################################################################
  # Reactive induced subgraph that is based on central_node() and input$centrality

  # Grabbing the subnetwork for the suspects
  G_passing_network <- reactive({
    induced.subgraph(sample_network,
                     vids = unlist(neighborhood(sample_network,
                                                order = input$degree_separation,
                                                nodes = central_node())))
  })

  ## Setting node size --------------------------------------------------------
  node_suspect_size <- reactive({

    if (input$centrality == 'Betweenness'){
      node_size <- data.frame('betweenness' = betweenness(G_passing_network(),
                                                                  v = V(G_passing_network()),
                                                                  directed = FALSE,
                                                                  normalized = TRUE)*10,
                              'UID' = V(G_passing_network())$name) %>%
        mutate(value = (betweenness / (max(betweenness) - min(betweenness)))*10) %>%
        select(UID, value)
    }
    else if (input$centrality == 'Closeness'){
      node_size <- data.frame('closeness' = closeness(G_passing_network(),
                                                              vids = V(G_passing_network()),
                                                              normalized = TRUE),
                              'UID' = V(G_passing_network())$name) %>%
        mutate(value = (closeness / (max(closeness) - min(closeness)))^4) %>%
        select(UID, value)
    }
    else if (input$centrality == 'Degree'){
      node_size <- data.frame('degree' = degree(G_passing_network(),
                                                        v = V(G_passing_network()),
                                                        loops = FALSE,
                                                        normalized = TRUE),
                              'UID' = V(G_passing_network())$name) %>%
        mutate(value = ((degree / (max(degree) - min(degree)))*10)^2) %>%
        select(UID, value)
    }
    else if (input$centrality == 'Eigen'){
      node_size <- data.frame(eigen_centrality(G_passing_network(),
                                                       directed = FALSE)) %>%
        mutate('UID' = V(G_passing_network())$name,
               value = (vector / (max(vector) - min(vector)))*20) %>%
        select(UID, value)
    }
    else {
      node_size <- data.frame('UID' = V(G_passing_network())$name,
                              'value' = 10)
    }

    node_size$UID <- as.character(node_size$UID)
    node_size$value <- as.integer(node_size$value)
    return(node_size)
  })


  ## Creating node and edge list ----------------------------------------------

  # Creating the node list
  node_suspect_list <- reactive({

    ids <- as_ids(V(G_passing_network()))

    df <- data.frame(x = ids) %>%
      separate(x, into = c('first', 'last', 'dob'), sep = '_') %>%
      unite(name, first, last, dob, sep = " ") %>%
      select(name)


  node_list <- data.frame(id = ids,
                          label = str_to_title(df$name))

  # Merging attributes that impact node shape to node list
  node_list <- left_join(node_list, suspect_attr, by = c('id' = 'UID'))

  # Joining afftributes that impact node size to node list
  node_list <- left_join(node_list, node_suspect_size(), by = c('id' = 'UID'))

  # Return the node list
  return(node_list)

  })

  # Creating the edge list
  edge_passing_list <- reactive ({

    # Creating data frame of edges from graph
    edge_list <- get.edgelist(G_passing_network()) %>%
      as.data.frame() %>%
      rename(from = V1, to = V2) %>%
      select(from, to) %>%
      distinct()

    # Return edge list
    return(edge_list)
  })

  ## Graphing Network ---------------------------------------------------------
  # The output$network_suspect visuazlies subgraphs with the following qualities:
  # 1) centrality = size of node
  # 2) crime type = shape of node (triangle if have committed; circle if not)

  my_network <- reactive({
    nodes <- node_suspect_list()
    edges <- edge_suspect_list()
    visNetwork(nodes, edges)
  })

  output$network_suspect <- renderVisNetwork({
    my_network() %>%
      visLegend(width = 0.1, position = 'right', main = '?',
                useGroups = FALSE, addNodes = data.frame(label = c('Yes', 'No'), 
                                                         shape = c('triangle', 'dot'))) %>%
      visExport(type = 'pdf', float = 'left', name = paste0(central_node(), '_network')) %>%
      visOptions(nodesIdSelection = TRUE,
                 highlightNearest = TRUE)

  })

  output$download_network <- downloadHandler(
    filename = function() {
      paste('network-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
      my_network() %>% visSave(con)
    }
  )

})
  
  
