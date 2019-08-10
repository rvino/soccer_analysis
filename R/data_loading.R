## Data loading for app

library(tidyverse)

# Lineup data load ------------------------------------------------------------
df_lineups <- read.csv('/Users/rorypulvino/Dropbox (Personal)/Python/Statsbomb/Data/NWSL_lineups.csv',
                       stringsAsFactors = FALSE)

team_name_list <- as.vector(unique(df_lineups$team.name))


# Pitch drawing functions for now ---------------------------------------------
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

# SB pitch dimensions
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

# Function to draw pitch
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