## Testing of a library to manipulate Statsbomb data and visualize in shiny
## loading libraries and data -------------------------------------------------
library(StatsBombR)
library(tidyverse)
library(stringr)

# df_events_sampled <- read.csv('/Users/rorypulvino/Dropbox (Personal)/Python/Statsbomb/Data/NWSL_sample.csv',
#                               stringsAsFactors = FALSE)

NWSL_matches <- read.csv('/Users/rorypulvino/Dropbox (Personal)/Python/Statsbomb/Data/NWSL_sample_matches.csv')
#df_events_sampled <- get.matchFree(NWSL_matches[1,]) # test
get_NWSL <- function(dataframe){
  Matches.df <- tibble()
  for(i in 1:nrow(dataframe)){
    matches <- get.matchFree(dataframe[i,])
    Matches.df <- bind_rows(Matches.df, matches)
  }

  return(Matches.df)
}
df_events_sampled <- get_NWSL(NWSL_matches)
df_events_sampled <- allclean(df_events_sampled)

# To be used for making a list of the teams available for mapping
team_name_list <- as.vector(unique(df_events_sampled$team.name))

## Drawing pitch with formations ----------------------------------------------
# sample shot data to graph
shots_sample_data <- df_events_sampled %>%
  filter(type.name=='Shot') %>%
  select(id:team.name, player.id:position.name, location.x, location.y,
         contains('Shot'))

# functions to plot field
pitch_theme <- ggplot2::theme(
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  axis.title       = ggplot2::element_blank(),
  axis.ticks       = ggplot2::element_blank(),
  axis.text        = ggplot2::element_blank(),
  axis.line        = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  panel.border     = ggplot2::element_blank()
)

pitch_dims <- list(
  length = 120,
  width = 80,
  penalty_box_length = 18,
  penalty_box_width = 44,
  six_yard_box_length = 6,
  six_yard_box_width = 20,
  penalty_spot_distance = 12,
  goal_width = 8,
  origin_x = 0,
  origin_y = 0)

draw_pitch <- function(pitch_lines, pitch_color){
  list(
    # rectangular border outside of the field
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$origin_x - 4,
      ymin = pitch_dims$origin_y - 4,
      xmax = pitch_dims$length + 4,
      ymax = pitch_dims$width + 4,
      fill = pitch_color
    ),
    # Drawing goals
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$origin_x - 2,
      ymin = (pitch_dims$width - pitch_dims$goal_width) / 2,
      xmax = pitch_dims$origin_x,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$goal_width) / 2),
      color = pitch_lines,
      fill = pitch_color
    ),
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$length,
      ymin = (pitch_dims$width - pitch_dims$goal_width) / 2,
      xmax = pitch_dims$length + 2,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$goal_width) / 2),
      color = pitch_lines,
      fill = pitch_color
    ),
    # drawing outline of field
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$origin_x,
      ymin = pitch_dims$origin_y,
      xmax = pitch_dims$length,
      ymax = pitch_dims$width,
      color = pitch_lines, 
      fill = pitch_color
      ),
    # Drawing center circle
    ggplot2::annotation_custom(
      grob = grid::circleGrob(gp = grid::gpar(col = pitch_lines,
                                              fill = pitch_color,
                                              lwd  = 2)),
      xmin = (pitch_dims$length / 2) - 12,
      xmax = (pitch_dims$length / 2) + 12,
      ymin = (pitch_dims$width / 2) - 12,
      ymax = (pitch_dims$width / 2) + 12
    ),
    # Drawing D
    ggplot2::annotation_custom(
      grob = grid::circleGrob(gp = grid::gpar(col = pitch_lines,
                                              fill = pitch_color,
                                              lwd  = 2)),
      xmin = pitch_dims$origin_x,
      xmax = pitch_dims$penalty_spot_distance + pitch_dims$penalty_spot_distance,
      ymin = (pitch_dims$width / 2) - pitch_dims$penalty_spot_distance,
      ymax = (pitch_dims$width / 2) + pitch_dims$penalty_spot_distance
    ),
    ggplot2::annotation_custom(
      grob = grid::circleGrob(gp = grid::gpar(col = pitch_lines,
                                              fill = pitch_color,
                                              lwd  = 2)),
      xmin = pitch_dims$length - (2*pitch_dims$penalty_spot_distance),
      xmax = pitch_dims$length,
      ymin = (pitch_dims$width / 2) - 12,
      ymax = (pitch_dims$width / 2) + 12
    ),
    # drawing midfield line
    ggplot2::annotate(
      geom = 'segment',
      x = pitch_dims$length / 2,
      y = pitch_dims$origin_y,
      xend = pitch_dims$length / 2,
      yend = pitch_dims$width,
      color = pitch_lines
      ),
    # drawing center point
    ggplot2::annotate(
      geom = 'point',
      x = pitch_dims$length / 2,
      y = pitch_dims$width / 2,
      color = pitch_lines
      ),
    # Drawing 18 yard box
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$origin_x,
      ymin = (pitch_dims$width - pitch_dims$penalty_box_width) / 2,
      xmax = pitch_dims$penalty_box_length,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$penalty_box_width) / 2),
      color = pitch_lines, 
      fill = pitch_color
    ),
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$length - pitch_dims$penalty_box_length,
      ymin = (pitch_dims$width - pitch_dims$penalty_box_width) / 2,
      xmax = pitch_dims$length,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$penalty_box_width) / 2),
      color = pitch_lines, 
      fill = pitch_color
    ),
    # Drawing 6 yard box
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$origin_x,
      ymin = (pitch_dims$width - pitch_dims$six_yard_box_width) / 2,
      xmax = pitch_dims$six_yard_box_length,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$six_yard_box_width) / 2),
      color = pitch_lines, 
      fill = pitch_color
    ),
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$length - pitch_dims$six_yard_box_length,
      ymin = (pitch_dims$width - pitch_dims$six_yard_box_width) / 2,
      xmax = pitch_dims$length,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$six_yard_box_width) / 2),
      color = pitch_lines, 
      fill = pitch_color
    ),
    # Drawing penalty spot
    ggplot2::annotate(
      geom = 'point',
      x = pitch_dims$penalty_spot_distance,
      y = pitch_dims$width / 2,
      color = pitch_lines
    ),
    ggplot2::annotate(
      geom = 'point',
      x = pitch_dims$length - pitch_dims$penalty_spot_distance,
      y = pitch_dims$width / 2,
      color = pitch_lines
    )
  )
}

# sample - plot of field
# ggplot(data=shots_sample_data, aes(x=location.x, y=location.y)) +
#   draw_pitch(pitch_lines = 'white', pitch_color = 'green')+
#   geom_point(color='blue')+
#   #coord_flip()+
#   pitch_theme

## Sample - Identify most common formation per team to plot ----
sb_lineups_ranked <- function(events_dataframe){
  df_lineup_ranked <- events_dataframe %>%
    filter(type.id==35) %>% # picking out formation data and limiting columns
    select(id, type.id, team.id:tactics.lineup) %>%
    unnest(., tactics.lineup) %>%
    group_by(id) %>%
    arrange(player.id) %>% # sort by player.id to make sure teams with same players but different formations are not ranked the same
    mutate(player = paste0('player', row_number())) %>%
    ungroup() %>%
    nest(jersey_number:position.name, .key = 'player_info') %>% # spreading player lineup data wide for match id
    spread(key = player, value = player_info) %>%
    unnest(., .sep = '_') %>%
    group_by_at(vars(team.id:player9_position.name)) %>%
    mutate(most_common_formation = n()) %>% # identifying the most common formation per team to plot
    ungroup() %>%
    group_by(team.id) %>%
    mutate(lineup.rank = rank(-most_common_formation, ties.method = 'first')) %>%
    ungroup() %>%
    select(id, type.id, team.id, lineup.rank)
}

df_events_sampled <- df_events_sampled %>%
  left_join(., sb_lineups_ranked(df_events_sampled), by = c('id', 'type.id',
                                                           'team.id'))
# dataframe of x, y positions to graph on a field based on the player position
player_positions <- data.frame(
  player_position = c('Goalkeeper', 
                      'Right Back', 'Left Back',
                      'Right Wing Back', 'Left Wing Back',
                      'Left Center Back', 'Right Center Back', 'Center Back',  
                      'Center Forward', 'Right Center Forward', 'Left Center Forward',
                      'Right Wing', 'Left Wing', 'Right Midfield', 'Left Midfield', 
                      'Left Defensive Midfield', 'Center Defensive Midfield', 'Right Defensive Midfield',  
                      'Center Midfield', 'Right Center Midfield', 'Left Center Midfield', 
                      'Center Attacking Midfield', 'Right Attacking Midfield', 'Left Attacking Midfield',
                      'Secondary Striker'),
  x = c(pitch_dims$length/10,
        rep(11*pitch_dims$length/60, 2), 
        rep(5*pitch_dims$length/24, 2),
        rep(11*pitch_dims$length/60, 3),
        rep(11*pitch_dims$length/24, 3),
        rep(5*pitch_dims$length/12, 2),
        rep(pitch_dims$length/3, 2),
        rep(4*pitch_dims$length/16, 3),
        rep(19*pitch_dims$length/60, 3),
        rep(9*pitch_dims$length/24, 3),
        5*pitch_dims$length/12),
  y = c(pitch_dims$width/2,
        pitch_dims$width/8, 7*pitch_dims$width/8,
        pitch_dims$width/16, 15*pitch_dims$width/16,
        11*pitch_dims$width/16, 5*pitch_dims$width/16,
        pitch_dims$width/2,
        pitch_dims$width/2, 5*pitch_dims$width/16,
        11*pitch_dims$width/16,
        pitch_dims$width/16, 15*pitch_dims$width/16,
        pitch_dims$width/8, 7*pitch_dims$width/8,
        5*pitch_dims$width/8, pitch_dims$width/2,
        3*pitch_dims$width/8, pitch_dims$width/2,
        11*pitch_dims$width/16, 5*pitch_dims$width/16,
        pitch_dims$width/2, 11*pitch_dims$width/16,
        7*pitch_dims$width/8,
        9*pitch_dims$width/16)
)

# Function to convert tactics.lineup into dataframe and give x,y positions
# for plotting
sample_formation_to_plot <- df_events_sampled %>%
  filter(type.id==35 & team.name=='Chicago Red Stars' & lineup.rank==1) %>% # picking out formation data and limiting columns
  select(id, team.id:tactics.lineup) %>%
  unnest(., tactics.lineup) %>%
  left_join(., player_positions, by = c('position.name'='player_position'))

ggplot(data=sample_formation_to_plot, aes(x=x, y=y)) +
  draw_pitch(pitch_lines = 'white', pitch_color = 'green')+
  geom_point(color='blue', size=8)+
  geom_text(aes(label=jersey_number), color='white')+
  #coord_flip()+
  pitch_theme

# Function to determine x and y points for points for formation - NB decided
# to hardcode the player positions since couldn't figure out a flexible 
# framework so this isn't necessary

# Function to create layer for plotting players with labeling
## Sample plot ##


