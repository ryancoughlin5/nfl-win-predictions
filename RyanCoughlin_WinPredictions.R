library(nflfastR)
library(nflreadr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(lubridate)
library(forecast)
options(scipen = 999)


df <- nflfastR::load_pbp(2015:2022, )
schedules <- nflreadr::load_schedules(2016:2022)
formations <- nflreadr::load_participation(2016:2022)


df$year <- substr(df$game_id, 1, 4)
df$year <- as.character(df$year)

formations$year <- substr(formations$nflverse_game_id, 1, 4)
formations$year <- as.character(formations$year)

#columns to keep from df
df_formations <- df[, c(1:5, 8, 10)]

formations <- inner_join(formations, df_formations, c('nflverse_game_id' = 'game_id', 'play_id' = 'play_id'))

formations <- formations[, c(1, 2, 15, 17:20, 4:14)]

# get rid of missing formations
formations <- formations[!is.na(formations$defense_personnel) | !is.na(formations$offense_personnel), ]

# count of most used personnel/formations
defensive_personnel <- formations %>% 
  group_by(defteam , year, defense_personnel) %>% 
  count(defense_personnel) %>% 
  arrange(defteam, year, desc(n))

offensive_personnel <- formations %>% 
       group_by(posteam , year, offense_personnel) %>% 
       count(offense_personnel) %>% 
       arrange(posteam, year, desc(n))

offense_formation <- formations %>% 
  group_by(posteam , year, offense_formation) %>% 
  count(offense_formation) %>% 
  arrange(posteam, year, desc(n))

defensive_personnel <- defensive_personnel %>% 
  group_by(defteam, year) %>% 
  mutate(row_num = row_number())

offensive_personnel <- offensive_personnel %>% 
  group_by(posteam, year) %>% 
  mutate(row_num = row_number())

offense_formation <- offense_formation %>% 
  group_by(posteam, year) %>% 
  mutate(row_num = row_number())

# only keep the most used personnel/formations
defensive_personnel <- defensive_personnel[defensive_personnel$row_num == 1, ]
offensive_personnel <- offensive_personnel[offensive_personnel$row_num == 1, ]
offense_formation <- offense_formation[offense_formation$row_num == 1, ]

typical_personnel <- inner_join(defensive_personnel, offensive_personnel, by=c('defteam'='posteam', 'year'='year'))
typical_personnel <- inner_join(typical_personnel, offense_formation, by=c('defteam'='posteam', 'year'='year'))
typical_personnel <- typical_personnel[, c(1:3, 6, 9)]

colnames(typical_personnel)[1] = 'Team'

# get rid of the blank rows
typical_personnel <- typical_personnel[!is.na(typical_personnel$Team), ]


#play_type_nfl, vegas_home_wp, vegas_wp, spread_line, home_wp, away_wp
# play_counts <- df %>% 
#      filter(play_type == 'run' | play_type == 'pass') %>% 
#      group_by(posteam, year, play_type) %>% 
#      mutate(count = n())

# 
# column_names <- tibble(colnames(df))
# 
# 
# weekly <- calculate_series_conversion_rates(df, weekly = TRUE)
# dplyr::glimpse(weekly)
# 
# overall <- calculate_series_conversion_rates(df, weekly = FALSE)
# dplyr::glimpse(overall)

# columns to keep
# cols_keep <- c('play_id', 'game_id', 'old_game_id', 'home_team', 'away_team', 'season_type', 'week', 'posteam', 
#                'posteam_type', 'defteam', 'side_of_field', 'yardline_100', 'game_date', 'quarter_seconds_remaining', 
#                'half_seconds_remaining', 'game_seconds_remaining', 'game_half', 'quarter_end', 'drive', 'sp', 'qtr',
#                'down', 'goal_to_go', 'time', 'yrdln', 'ydstogo', 'ydsnet', 'play_type', 'posteam_timeouts_remaining', 
#                'defteam_timeouts_remaining', 'posteam_score', 'defteam_score', 'score_differential', 'posteam_score_post',
#                'defteam_score_post', 'score_differential_post', 'fourth_down_converted', 'fourth_down_failed', 'season', 'series',
#                'order_sequence', 'start_time', 'time_of_day', 'stadium', 'weather', 'div_game', 'home_coach', 'away_coach', 
#                'stadium_id', 'game_stadium', 'field_goal_result', 'kick_distance', 'field_goal_attempt', 'series_result',
#                'punt_attempt', 'epa', 'success')
# 
# pbp <- df[cols_keep]


#### Get Team Records ####
schedules$win_loss <- if_else(schedules$result > 0, 'home_win', 
                              if_else(schedules$result < 0, 'away_win', 'tie'))


home_wins <- schedules %>% 
  filter(win_loss == 'home_win',  game_type == 'REG') %>% 
  group_by(home_team, season) %>%
  summarise(wins = n(), points = sum(home_score)) %>% 
  rename('team' = 'home_team') 
  

home_losses <- schedules %>% 
  filter(win_loss == 'away_win', game_type == 'REG') %>% 
  group_by(home_team, season) %>% 
  summarise(losses = n(), points = sum(home_score)) %>% 
  rename('team' = 'home_team')

home_ties <- schedules %>% 
  filter(win_loss == 'tie', game_type == 'REG') %>% 
  group_by(home_team, season) %>% 
  summarise(ties = n(), points = sum(home_score)) %>%
  rename('team' = 'home_team')

away_wins <- schedules %>% 
  filter(win_loss == 'away_win', game_type == 'REG') %>% 
  group_by(away_team, season) %>% 
  summarise(wins = n(), points = sum(away_score)) %>%
  rename('team' = 'away_team')

away_losses <- schedules %>% 
  filter(win_loss == 'home_win', game_type == 'REG') %>% 
  group_by(away_team, season) %>% 
  summarise(losses = n(), points = sum(away_score)) %>%
  rename('team' = 'away_team')

away_ties <- schedules %>% 
  filter(win_loss == 'tie', game_type == 'REG') %>% 
  group_by(away_team, season) %>% 
  summarise(ties = n(), points = sum(away_score)) %>%
  rename('team' = 'away_team')

# win_loss <- list(home_wins, home_losses, home_ties, away_wins, away_losses, away_ties)

# win_loss <- win_loss %>% reduce(full_join, by=c('team', 'season'))

wins <- full_join(home_wins, away_wins, by=c('team', 'season'))
wins$wins.x <- coalesce(wins$wins.x, 0)
wins$wins.y <- coalesce(wins$wins.y, 0)
wins$wins <- wins$wins.x + wins$wins.y

wins$points.x <- coalesce(wins$points.x, 0)
wins$points.y <- coalesce(wins$points.y, 0)
wins$points_win <- wins$points.x + wins$points.y

# get rid of these columns
wins <- wins[, !names(wins) %in% c('points.x', 'points.y', 'wins.x', 'wins.y')]

losses <- full_join(home_losses, away_losses, by=c('team', 'season'))
losses$losses.x <- coalesce(losses$losses.x, 0)
losses$losses.y <- coalesce(losses$losses.y, 0)
losses$losses <- losses$losses.x + losses$losses.y

losses$points.x <- coalesce(losses$points.x, 0)
losses$points.y <- coalesce(losses$points.y, 0)
losses$points_loss <- losses$points.x + losses$points.y

losses <- losses[, !names(losses) %in% c('points.x', 'points.y', 'losses.x', 'losses.y')]

ties <- full_join(home_ties, away_ties, by=c('team', 'season'))  
ties$ties.x <- coalesce(ties$ties.x, 0)
ties$ties.y <- coalesce(ties$ties.y, 0)
ties$ties <- ties$ties.x + ties$ties.y

ties$points.x <- coalesce(ties$points.x, 0)
ties$points.y <- coalesce(ties$points.y, 0)
ties$points_tie <- ties$points.x + ties$points.y

ties <- ties[, !names(ties) %in% c('points.x', 'points.y', 'ties.x', 'ties.y')]

team_info <- full_join(wins, losses, by=c('team', 'season'))

team_info <- full_join(team_info, ties, by=c('team', 'season'))

team_info$wins <- coalesce(team_info$wins, 0)
team_info$losses <- coalesce(team_info$losses, 0)
team_info$ties <- coalesce(team_info$ties, 0)
team_info$points_win <- coalesce(team_info$points_win, 0)
team_info$points_loss <- coalesce(team_info$points_loss, 0)
team_info$points_tie <- coalesce(team_info$points_tie, 0)

team_info$total_points <- team_info$points_win + team_info$points_loss + team_info$points_tie

#team_info <-  team_info[c('team', 'season', 'wins', 'losses', 'ties', 'points')]

# change team names that have changed in the last 10 years
team_info <- inner_join(team_info, teams_colors_logos, by=c('team' = 'team_abbr'))
team_info$team[team_info$team == 'STL'] = 'LA'
team_info$team_name[team_info$team_name == 'St. Louis Rams'] = 'Los Angeles Rams'

team_info$team[team_info$team == 'SD'] = 'LAC'
team_info$team_name[team_info$team_name == 'San Diego Chargers'] = 'Los Angeles Chargers'

team_info$team[team_info$team == 'OAK'] = 'LV'
team_info$team_name[team_info$team_name == 'Oakland Raiders'] = 'Las Vegas Raiders'


##### Join Team Data and Personnel Data ####
team_info$season <- as.character(team_info$season)
teams_df <- inner_join(typical_personnel, team_info[, c(1:11)], by=c('Team'='team', 'year'='season'))

#average points per game
teams_df$ppg <- if_else(teams_df$year == '2022', round(teams_df$total_points/17, 2), round(teams_df$total_points/16, 2))

#### get betting and odds data from pbp data)

#play_type_nfl, vegas_home_wp, vegas_wp, spread_line, home_wp, away_wp
# play_counts <- df %>% 
#      filter(play_type == 'run' | play_type == 'pass') %>% 
#      group_by(posteam, year, play_type) %>% 
#      mutate(count = n())

#create odds tables
home_odds <- df[df$play_type_nfl == 'GAME_START' & df$season_type == 'REG', 
              c(which(colnames(df)=='home_team'), 
               which(colnames(df)=='year'),
               which(colnames(df)=='week'),
               which(colnames(df)=='season_type'),
               which(colnames(df)=='play_type_nfl'),
               which(colnames(df)=='spread_line'), 
               which(colnames(df)=='vegas_home_wp'))]

away_odds <- df[df$play_type_nfl == 'GAME_START' & df$season_type == 'REG', 
                c(which(colnames(df)=='away_team'), 
                  which(colnames(df)=='year'),
                  which(colnames(df)=='week'),
                  which(colnames(df)=='season_type'),
                  which(colnames(df)=='play_type_nfl'),
                  which(colnames(df)=='spread_line'),
                  which(colnames(df)=='vegas_home_wp'))]


away_odds$vegas_away_wp <- 1-away_odds$vegas_home_wp #create vegas away odds from vegas home odds
away_odds$spread_line <- (away_odds$spread_line)*(-1) #flip the sign for the spread line so it's the opposite of the home odds dataset
away_odds <- away_odds[, !names(away_odds) %in% c('vegas_home_wp')] # remove vegas home odds column

home_odds <- home_odds %>% 
  group_by(home_team, year) %>% 
  summarise(avg_spread = round(mean(spread_line), 2), avg_vegas_home_wp = round(mean(vegas_home_wp), 2))


away_odds <- away_odds %>% 
       group_by(away_team, year) %>% 
       summarise(avg_spread = round(mean(spread_line), 2), avg_vegas_away_wp = round(mean(vegas_away_wp), 2))


odds <- inner_join(home_odds, away_odds, c('home_team'='away_team', 'year'='year'))

odds$avg_spread <- (odds$avg_spread.x +odds$avg_spread.y)/2

colnames(odds)[1] = 'Team'
colnames(odds)[3] = 'home_spread'
colnames(odds)[5] = 'away_spread'

team_info <- inner_join(odds, team_info, by=c('Team'='team', 'year'='season'))

head_coaches <- df %>% 
  group_by(home_team, year) %>% 
  filter(play_type_nfl=='GAME_START') %>% 
  select(home_team, year, home_coach, play_type_nfl) %>% 
  distinct() %>% 
  mutate(row_num = row_number()) %>% 
  arrange(home_team, year)

head_coaches <- head_coaches[head_coaches$row_num == 1, ] #only keep the coaches who started the year
head_coaches <- head_coaches[, !names(head_coaches) %in% c('play_type_nfl', 'row_num')] #get rid of play type

# make a new column to see the coach from the previous year
head_coaches <- head_coaches %>% 
  group_by(home_team) %>% 
  mutate(previous_coach = lag(home_coach, n=1)) 

head_coaches <- head_coaches[head_coaches$year > 2015, ] # get rid of 2015

head_coaches$new_coach <- if_else(head_coaches$home_coach==head_coaches$previous_coach, 0, 1) #flag if there's a new coach from previous year

colnames(head_coaches)[1] = 'Team'
colnames(head_coaches)[3] = 'Coach'

head_coaches <- head_coaches[, !names(head_coaches) %in% c('previous_coach')] #get rid of play type

team_info <- inner_join(head_coaches, team_info, by=c('Team'='Team', 'year'='year'))


#### Regression ####
vars <- c(4:16, 18)

set.seed(5)
train_df <- team_info[team_info$year <= 2020, vars]
validate_df <- team_info[team_info$year > 2020, vars]

team_info_lm <- lm(wins ~., data=train_df)
summary(team_info_lm)

#### Predictions ####
team_info_lm_pred <- predict(team_info_lm, validate_df)
some_residuals <- validate_df$wins[1:14] - team_info_lm_pred[1:14]
view(data.frame("Predicted" = team_info_lm_pred[1:14], "Actual" = validate_df$wins[1:14], "Residual" = some_residuals))

accuracy(team_info_lm_pred, validate_df$wins)





#### 


#### Player Data no 2022 as far as I can tell####
# player_info <- load_players()
# team_ids <- team_info[c('team', 'team_name', 'team_id')]
# player_info <-  inner_join(player_info, team_ids, by=c('current_team_id' = 'team_id'))
# player_info$age <- interval(end=today(), start=player_info$birth_date)/duration(n=1, unit='years')
# player_info$age <- round(player_info$age, 0)
# 
# player_info_2022 <- subset(player_info, season == 2022)
# 
# 
# player_info_2022 %>% 
#   group_by(team) %>% 
#   summarise(avg_age = mean(age))
# player stats and rosters
# player_stats <- load_player_stats(season=c(2016:2022))
# rosters <- load_rosters(season=c(2016:2022))
# 
# player_stats$recent_team[player_stats$recent_team == 'STL'] = 'LA'
# 
# player_stats$recent_team[player_stats$recent_team == 'SD'] = 'LAC'
# 
# player_stats$recent_team[player_stats$recent_team == 'OAK'] = 'LV'
# 
# qb_stats <- player_stats %>% 
#   group_by(player_display_name, recent_team, position, season) %>% 
#   filter(position_group == 'QB') %>% 
#   summarise(total_attempts = sum(attempts), total_yards = sum(passing_yards), total_epa = round(sum(passing_epa), 0), predicted_epa_next = round(sum(dakota), 0))
# 
# qb_stats <-  inner_join(qb_stats, team_info[c('team', 'team_color', 'season')], by=c('recent_team' = 'team', 'season' = 'season'))
# 
# rb_stats <- player_stats %>% 
#   group_by(player_display_name, recent_team, position, season) %>% 
#   filter(position_group == 'RB') %>% 
#   summarise(total_yards = sum(rushing_yards), total_epa = round(sum(rushing_epa), 0))
# 
# receiving_stats <- player_stats %>% 
#   group_by(player_display_name, recent_team, position, season) %>% 
#   filter(position_group == 'WR' | position_group == 'TE') %>% 
#   summarise(total_yards = sum(receiving_yards), total_epa = round(sum(receiving_epa), 0))





### Start aggregating and visualizing ####
# team_info %>% 
#   select(team_name, wins, losses, ties, team_color, team_color2) %>% 
#   group_by(team_name, team_color, team_color2) %>% 
#   summarise(avg_wins = mean(wins), avg_losses = mean(losses), avg_tiess = mean(ties)) %>% 
#   plot_ly(x=~team_name, 
#           y=~avg_wins, 
#           marker=list(color=~team_color)) %>% 
#   layout(xaxis = list(categoryorder = "total descending", title='Team'),
#          yaxis = list(title='Average Wins')) 
# 
# team_info %>% 
#   select(team, wins, losses, ties, team_color, team_color2) %>% 
#   group_by(team, team_color, team_color2) %>% 
#   summarise(avg_wins = mean(wins), avg_losses = mean(losses), avg_tiess = mean(ties)) %>% 
#   plot_ly(x=~team, 
#           y=~avg_losses, 
#           marker=list(color=~team_color)) %>% 
#   layout(xaxis = list(categoryorder = "total descending", title='Team'),
#          yaxis = list(title='Average Losses')) 
#   
# # 2022 Season
# season_2022 <- team_info[team_info$season == '2022', ]
# 
# # Avg PPG Points for 2022
# ggplot(season_2022, aes(x=reorder(team, desc(total_points)), y=total_points, color=team_color2, fill=team_color)) + 
#   geom_col(size=1) +
#   scale_color_identity() +
#   scale_fill_identity() +
#   ylab('Points') +
#   ggtitle("Average Points Per Game - 2022") +
#   theme_classic() +
#   theme(axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
#   geom_text(aes(label=round(total_points/17, 1), vjust=3))
# 
# 
# # total wins for 2022
# ggplot(season_2022, aes(x=reorder(team, desc(wins)), y=wins, color=team_color2, fill=team_color)) + 
#   geom_col(size=1) +
#   scale_color_identity() +
#   scale_fill_identity() +
#   ylab('2022 Wins') +
#   ggtitle("2022 Win Totals") +
#   theme_classic() +
#   theme(axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
#   geom_text(aes(label=wins, vjust=3))
# 
# 
# #### "Average Regular Season Wins Since 2010" ####
# avg_wins <- team_info %>% 
#   select(team, team_division, wins, losses, ties, team_color, team_color2) %>% 
#   group_by(team, team_division, team_color, team_color2, ) %>% 
#   summarise(avg_wins = mean(wins), avg_losses = mean(losses), avg_tiess = mean(ties)) 
# 
# # avg wins per season
# ggplot(avg_wins, aes(x=reorder(team, desc(avg_wins)), y=avg_wins, color=team_color2, fill=team_color)) + 
#   geom_col(size=1) +
#   scale_color_identity() +
#   scale_fill_identity() +
#   ylab('Avergae Wins') +
#   ggtitle("Average Regular Season Wins Since 2010") +
#   theme_classic() +
#   theme(axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
#   geom_text(aes(label=round(avg_wins, 2), vjust=3))
# 
# 
# ggplot(avg_wins, aes(x=reorder(team, desc(avg_wins)), y=avg_wins, color=team_color2, fill=team_color)) + 
#   geom_col(size=1) +
#   scale_color_identity() +
#   scale_fill_identity() +
#   ylab('Avergae Wins') +
#   ggtitle("Average Regular Season Wins Since 2010") +
#   theme_classic() +
#   theme(axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
#   geom_text(aes(label=round(avg_wins, 2), vjust=3)) + 
#   facet_wrap(~team_division, scales='free')
# 
# ## player stats graphs
# qb_stats_over_350 <- qb_stats[qb_stats$total_attempts > 350, ]
# qb_stats_over_350 <- qb_stats_over_350[order(qb_stats_over_350$total_yards, decreasing=T),]
# list(qb_stats_over_350[1:10, 1])
# qb_stats_over_350_top10 <- qb_stats_over_350[qb_stats_over_350$player_display_name %in% c('Matthew Stafford',
#                                                                                                   'Patrick Mahomes',  
#                                                                                                   'Tom Brady',
#                                                                                                   'Joe Burrow',
#                                                                                                   'Josh Allen',
#                                                                                                   'Derek Carr'), ]
# 
# ggplot(qb_stats_over_350_top10 , aes(x=season, y=total_yards, color=team_color, group=player_display_name))+
#   geom_line()+
#   scale_color_identity() +
#   theme_classic() + 
#   geom_text(aes(label= player_display_name))+
#   ylab('Total Yards') + 
#   xlab('Season')+
#   ggtitle('Most Yards Since 2019')+
#   theme(plot.title = element_text(hjust = 0.5))
# 








