library(nflverse)
library(tidyverse)
pbp <- nflfastR::load_pbp(2010:2022)

column_names <- tibble(colnames(pbp))

# columns to keep
cols_keep <- c('play_id', 'game_id', 'old_game_id', 'home_team', 'away_team', 'season_type', 'week', 'posteam', 
               'posteam_type', 'defteam', 'side_of_field', 'yardline_100', 'game_date', 'quarter_seconds_remaining', 
               'half_seconds_remaining', 'game_seconds_remaining', 'game_half', 'quarter_end', 'drive', 'sp', 'qtr',
               'down', 'goal_to_go', 'time', 'yrdln', 'ydstogo', 'ydsnet', 'play_type', 'posteam_timeouts_remaining', 
               'defteam_timeouts_remaining', 'posteam_score', 'defteam_score', 'score_differential', 'posteam_score_post',
               'defteam_score_post', 'score_differential_post', 'fourth_down_converted', 'fourth_down_failed', 'season', 'series',
               'order_sequence', 'start_time', 'time_of_day', 'stadium', 'weather', 'div_game', 'home_coach', 'away_coach', 
               'stadium_id', 'game_stadium')

pbp <- pbp[cols_keep]
go_for_it <- pbp %>% filter(down == '4' & (play_type %in% c('pass', 'run')))
all_fourth <- pbp %>%  filter(down == '4')

mean(go_for_it$ydstogo)
median(go_for_it$ydstogo)

# percent of time teams go for it on 4th down
count(go_for_it[[]])/count(all_fourth)*100


#### Get total times a coach has gone for it ####
home_coach_go <- go_for_it %>%
  rename('coach' = 'home_coach') %>% 
  group_by(coach) %>% 
  filter(posteam_type == 'home') %>% 
  count(coach, name = 'count_home')

away_coach_go <- go_for_it %>%
  rename('coach' = 'away_coach') %>% 
  group_by(coach) %>% 
  filter(posteam_type == 'away') %>% 
  count(coach, name = 'count_away')

go_for_it_coach <- merge(away_coach_go, home_coach_go, by = 'coach')
go_for_it_coach$go = go_for_it_coach$count_home + go_for_it_coach$count_away
go_for_it_coach <- go_for_it_coach[, c(1, 4)]

#### All 4th downs in a coaches career ####
home_coach_4 <- all_fourth %>%
  rename('coach' = 'home_coach') %>% 
  group_by(coach) %>% 
  filter(posteam_type == 'home') %>% 
  count(coach, name = 'count_home')

away_coach_4 <- all_fourth %>%
  rename('coach' = 'away_coach') %>% 
  group_by(coach) %>% 
  filter(posteam_type == 'away') %>% 
  count(coach, name = 'count_away')

all_fourth_coach <- merge(away_coach_4, home_coach_4, by = 'coach')
all_fourth_coach$all = all_fourth_coach$count_home + all_fourth_coach$count_away
all_fourth_coach <- all_fourth_coach[, c(1, 4)]

#### Merge go_for_it_coach and all_fourth_coach to get 4th down go percentage ####
go_rates <- merge(go_for_it_coach, all_fourth_coach, by='coach')
go_rates$go_percentage <- round((go_rates$go/go_rates$all) * 100, 2)
go_rates <- (go_rates[order(go_rates$go_percentage, decreasing = T), ]) 
go_rates <- mutate(go_rates, rank = row_number())
