library(StatsBombR)
library(tidyverse)
library(stringr)
library()

## read in data ---------------------------------------------------------------
# read in events
df_events <- StatsBombFreeEvents()

# read in competitions
df_comps <- FreeCompetitions()

# read in matches
df_matches <- FreeMatches(df_comps$competition_id)

## Cleaning data --------------------------------------------------------------
# Cleans all the data
df_events_cleaned <- allclean(df_events)
# Checking the differences in columns from this and the copy of df_events
setdiff(colnames(df_events_cleaned), colnames(df_events))
# the all clean kept the same row structure to the data but added 34 variables

## Filtering to some sample data ----------------------------------------------
nwsl_teams <- c('Portland Thorns', 'Seattle Reign', 'North Carolina Courage',
                'Chicago Red Stars', 'Utah Royals', 'Washington Spirit',
                'Sky Blue FC', 'Orlando Pride', 'Houston Dash')

df_events_sampled <- filter(df_events_cleaned, team.name %in% nwsl_teams)

rm(df_events, df_events_cleaned)

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

draw_pitch <- function(pitch_lines,
                             pitch_color){
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

# sample plot of field
# ggplot(data=shots_sample_data, aes(x=location.x, y=location.y)) +
#   draw_pitch(pitch_lines = 'white', pitch_color = 'green')+
#   geom_point(color='blue')+
#   #coord_flip()+
#   pitch_theme
  
# Identify most common formation per team to plot
formation_sample_data <- formation_sample_data %>%
  group_by(team.name, tactics.formation) %>%
  mutate(most_common_formation = n()) %>%
  ungroup() %>%
  group_by(team.name) %>%
  mutate(Rank = rank(-most_common_formation, ties.method = 'first'))

# dataframe of x, y positions to graph on a field based on the player position
player_positions <- data.frame(
  player_position = c('Goalkeeper', 'Right Center Back', 'Center Back',
                      'Left Center Back', 'Right Defensive Midfield',
                      'Left Defensive Midfield', 'Right Midfield', 
                      'Left Midfield', 'Center Attacking Midfield',
                      'Right Center Forward', 'Left Center Forward',
                      'Right Back', 'Left Back', 'Right Center Midfield',
                      'Left Center Midfield', 'Right Wing', 'Left Wing',
                      'Center Forward', 'Center Defensive Midfield',
                      'Secondary Striker', 'Center Midfield',
                      'Right Attacking Midfield', 'Left Attacking Midfield',
                      'Right Wing Back', 'Left Wing Back'), 
  x = c(pitch_dims$penalty_spot_distance, pitch_dims$length/6, 
        pitch_dims$length/6, pitch_dims$length/6, pitch_dims$length/4,
        pitch_dims$length/4, pitch_dims$length/3, pitch_dims$length/3, 
        (5*pitch_dims$length)/12, pitch_dims$length/2, pitch_dims$length/2,
        (5*pitch_dims$length)/24, (5*pitch_dims$length)/24, 
        pitch_dims$length/3, pitch_dims$length/3, (9*pitch_dims$length)/24,
        (9*pitch_dims$length)/24, pitch_dims$length/2, pitch_dims$length/4,
        (11*pitch_dims$length)/24, pitch_dims$length/3, (5*pitch_dims$length)/12,
        (5*pitch_dims$length)/12, (7*pitch_dims$length)/24,
        (7*pitch_dims$length)/24),
  y = c(pitch_dims$width/2, (3*pitch_dims$width)/8, pitch_dims$width/2, 
        (5*pitch_dims$width)/8, (3*pitch_dims$width)/8,
        (5*pitch_dims$width)/8, pitch_dims$width/8, (7*pitch_dims$width)/8, 
        pitch_dims$width/2, (3*pitch_dims$width)/8, (5*pitch_dims$width)/8, 
        pitch_dims$width/8, (7*pitch_dims$width)/8, (5*pitch_dims$width)/8, 
        (3*pitch_dims$width)/8, pitch_dims$width/16, (15*pitch_dims$width)/16,
        pitch_dims$width/2, pitch_dims$width/2, pitch_dims$width/2, 
        pitch_dims$width/2, (3*pitch_dims$width)/8, (5*pitch_dims$width)/8,
        pitch_dims$width/16, (15*pitch_dims$width)/16)
)

# Function to convert tactics.lineup into dataframe and identify most common
# team formation
statsbomb_lineup <- function(data){
  df_lineups <- data %>%
    filter(type.name=='Starting XI') %>% # picking out formation data and limiting columns
    select(id, team.id:tactics.lineup) %>%
    unnest(., tactics.lineup) %>%
    full_join(., player_positions, by = c('position.name'='player_position')) %>%
    group_by(id) %>%
    mutate(player = paste0('player', row_number())) %>%
    ungroup() %>%
    gather() %>% # spreading player lineup data wide for match id
    unite() %>%
    spread(key = player, value = ) %>% 
    group_by(team.id:player11) %>% 
    mutate(most_common_formation = n()) %>% # identifying the most common formation per team to plot
    ungroup() %>%
    group_by(team.id) %>%
    mutate(Rank = rank(-most_common_formation, ties.method = 'first')) %>%
    ungroup()
}

formation_sample_data <- statsbomb_lineup(df_events_sampled)

# Function to determine x and y points for points for formation - NB decided
# to hardcode the player positions since couldn't figure out a flexible 
# framework

# Function to create layer for plotting players with labeling



