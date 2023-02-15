library(nflverse)
library(tidyverse)
pbp <- nflfastR::load_pbp(2010:2022)

go_for_it <- pbp %>% filter(down == '4' & (play_type %in% c('pass', 'run')))

mean(go_for_it$ydstogo)
median(go_for_it$ydstogo)

column_names <- tibble(colnames(pbp))

cols_keep <- c('play_id', 'game_id', 'old_game_id', 'home_team', 'away_team', 'season_type', 'week', 'posteam', 
               'posteam_type', 'defteam', 'side_of_field', 'yardline_100', 'game_date', 'quarter_seconds_remaining', 
               'half_seconds_remaining', 'game_seconds_remaining', 'game_half', 'quarter_end', 'drive', 'sp', 'qtr',
               'down', 'goal_to_go', 'time', 'yrdln', 'ydstogo', 'ydsnet', 'play_type', 'yards_gained', 'shotgun', 'no_huddle')