library(nflfastR)
library(nflreadr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(lubridate)
library(forecast)
library(leaps) 
library(zoo)
options(scipen = 999)


df <- nflfastR::load_pbp(2010:2022)
schedules <- nflreadr::load_schedules(2000:2022)
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

team_info <- team_info %>% 
  group_by(team) %>% 
  mutate(last_year = lag(wins, n=1), 
         two_year_record = rollmean(lag(wins, n=1), 2, align='right', fill='NA'),
         five_year_record = rollmean(lag(wins, n=1), 5, align='right', fill='NA'), 
         ten_year_record = rollmean(lag(wins, n=1), 10, align='right', fill='NA'))

##### Join Team Data and Personnel Data ####
team_info$season <- as.character(team_info$season)
teams_df <- inner_join(typical_personnel, team_info[, c(1:11, 26, 25, 27, 24)], by=c('Team'='team', 'year'='season'))

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



teams_df <- inner_join(odds, teams_df, by=c('Team'='Team', 'year'='year'))

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

teams_df <- inner_join(head_coaches, teams_df, by=c('Team'='Team', 'year'='year'))

teams_df$defense_personnel <- as.factor(teams_df$defense_personnel)
#teams_df$avg_vegas_home_wp <- teams_df$avg_vegas_home_wp/100
#teams_df$avg_vegas_away_wp <- teams_df$avg_vegas_away_wp/100
teams_df$ppg <- round(teams_df$ppg, 0)
teams_df$offense_personnel <- if_else(teams_df$offense_personnel == '2 RB, 2 TE, 1 WR', 'Other', teams_df$offense_personnel)

#### Regression ####
vars <- c(5:8, 10, 12, 13, 19, 22:26)

set.seed(5)
train_df <- teams_df[teams_df$year <= 2020, vars]
validate_df <- teams_df[teams_df$year > 2020, vars]

wins_lm <- lm(wins ~., data=train_df)
summary(wins_lm)

#### Predictions ####
wins_lm_pred <- predict(wins_lm, validate_df)

some_residuals <- validate_df$wins[1:14] - wins_lm_pred[1:14]
view(data.frame("Predicted" = round(wins_lm_pred[1:14], 0), "Actual" = validate_df$wins[1:14], "Residual" = some_residuals))

accuracy(wins_lm_pred, validate_df$wins)

all_residuals <- validate_df$wins - wins_lm_pred

length(all_residuals[which(all_residuals > -1.554  & all_residuals < 1.554)])/64

hist(all_residuals,xlab = "Residuals", main = "")

wins_pred <- as.data.frame(wins_lm_pred)
# cbind(wins_pred, validate_df)
#### Exhaustive Search ####

search <-  regsubsets(wins ~., data=train_df, nbest=1, nvmax=dim(train_df)[2], method='exhaustive')
dim(train_df)
sum <- summary(search)
view(sum$which)

# create indicators out of my factor column to take out insignificant rows
sum$rsq
sum$adjr2
sum$cp
plot(search)
plot(search, scale='adjr2')

#### Take Home and away spread out ####
teams_df$defense_personnel <- if_else(teams_df$defense_personnel == '2 DL, 3 LB, 6 DB' | teams_df$defense_personnel == '3 DL, 4 LB, 4 DB', 
                                      teams_df$defense_personnel, 
                                      'Other')

vars <- c(which(colnames(teams_df)=='wins'), 
          which(colnames(teams_df)=='home_spread'),
          which(colnames(teams_df)=='avg_vegas_home_wp'),
          which(colnames(teams_df)=='away_spread'),
          which(colnames(teams_df)=='avg_vegas_away_wp'), 
          which(colnames(teams_df)=='defense_personnel'), 
          which(colnames(teams_df)=='ppg'))

set.seed(5)
train_df <- teams_df[teams_df$year <= 2020, vars]
validate_df <- teams_df[teams_df$year > 2020, vars]

wins_lm <- lm(wins ~., data=train_df)
summary(wins_lm)

#### Predictions ####
wins_lm_pred <- predict(wins_lm, validate_df)

some_residuals <- validate_df$wins[1:14] - wins_lm_pred[1:14]
view(data.frame("Predicted" = wins_lm_pred[1:14], "Actual" = validate_df$wins[1:14], "Residual" = some_residuals))

accuracy(wins_lm_pred, validate_df$wins)

all_residuals <- validate_df$wins - wins_lm_pred

length(all_residuals[which(all_residuals > -1.551  & all_residuals < 1.551)])/64

hist(all_residuals,xlab = "Residuals", main = "")

wins_pred <- as.data.frame(wins_lm_pred)



#### Reduce vars again ####
vars <- c(which(colnames(teams_df)=='wins'), 
          which(colnames(teams_df)=='home_spread'),
          which(colnames(teams_df)=='avg_vegas_home_wp'),
          which(colnames(teams_df)=='away_spread'),
          which(colnames(teams_df)=='avg_vegas_away_wp'), 
          which(colnames(teams_df)=='ppg'),
          which(colnames(teams_df)=='total_points'))

set.seed(5)
train_df <- teams_df[teams_df$year <= 2020, vars]
validate_df <- teams_df[teams_df$year > 2020, vars]

wins_lm <- lm(wins ~., data=train_df)
summary(wins_lm)

#### Predictions ####
wins_lm_pred <- predict(wins_lm, validate_df)

some_residuals <- validate_df$wins[1:14] - wins_lm_pred[1:14]
view(data.frame("Predicted" = wins_lm_pred[1:14], "Actual" = validate_df$wins[1:14], "Residual" = some_residuals))

accuracy(wins_lm_pred, validate_df$wins)

all_residuals <- validate_df$wins - wins_lm_pred

length(all_residuals[which(all_residuals > -1.555  & all_residuals < 1.555)])/64

hist(all_residuals,xlab = "Residuals", main = "")

wins_pred <- as.data.frame(wins_lm_pred)

## Backward ##
vars <- c(5:8, 10, 12, 13, 19, 22:26)
set.seed(5)
train_df <- na.omit(teams_df[teams_df$year <= 2020, vars])

validate_df <- na.omit(teams_df[teams_df$year > 2020, vars])

wins_lm <- lm(wins ~., data=train_df)
summary(wins_lm)
wins_lm_step <- stats::step(wins_lm, direction = 'backward')
wins_lm_step_pred <- predict(wins_lm_step, validate_df)
summary(wins_lm_step)

some_residuals <- validate_df$wins[1:14] - wins_lm_step_pred[1:14]
view(data.frame("Predicted" = wins_lm_step_pred[1:14], "Actual" = validate_df$wins[1:14], "Residual" = some_residuals))

accuracy(wins_lm_step_pred, validate_df$wins)

all_residuals <- validate_df$wins - wins_lm_step_pred

length(all_residuals[which(all_residuals > -1.555  & all_residuals < 1.555)])/64

hist(all_residuals,xlab = "Residuals", main = "")

wins_pred <- as.data.frame(wins_lm_step_pred)

#### Step-wise Regression ####
## Both ##
vars <- c(5:8, 10, 12, 13, 19, 22:26)
set.seed(5)
train_df <- teams_df[teams_df$year <= 2020, vars]
train_df <- na.omit(train_df)
validate_df <- teams_df[teams_df$year > 2020, vars]

wins_lm <- lm(wins ~., data=train_df)
summary(wins_lm)
wins_lm_step <- stats::step(wins_lm, direction = 'both')
wins_lm_step_pred <- predict(wins_lm_step, validate_df)
summary(wins_lm_step)

## Forward ##
vars <- c(5:8, 10, 12, 13, 19, 22:26)
set.seed(5)
train_df <- teams_df[teams_df$year <= 2020, vars]
validate_df <- teams_df[teams_df$year > 2020, vars]

wins_lm <- lm(wins ~., data=train_df)
summary(wins_lm)
wins_lm_step <- stats::step(wins_lm, direction = 'forward')
wins_lm_step_pred <- predict(wins_lm_step, validate_df)
summary(wins_lm_step)

some_residuals <- validate_df$wins[1:14] - wins_lm_step_pred[1:14]
view(data.frame("Predicted" = wins_lm_step_pred[1:14], "Actual" = validate_df$wins[1:14], "Residual" = some_residuals))

accuracy(wins_lm_step_pred, validate_df$wins)

all_residuals <- validate_df$wins - wins_lm_step_pred

length(all_residuals[which(all_residuals > -1.555  & all_residuals < 1.555)])/64

hist(all_residuals,xlab = "Residuals", main = "")

wins_pred <- as.data.frame(wins_lm_step_pred)

#### Predicting 2023 Wins
# vars <- c(5:8, 10, 12, 13, 19, 22)
# set.seed(5)
# train_df <- teams_df[teams_df$year <= 2020, vars]
# validate_df <- teams_df[teams_df$year > 2020, vars]
# 
# wins_lm <- lm(wins ~., data=train_df)
# summary(wins_lm)
# wins_lm_step <- stats::step(wins_lm, direction = 'backward')
# wins_lm_step_pred <- predict(wins_lm_step)
# summary(wins_lm_step)






