# Scrape data to use for project

library(hoopR)
library(tidyverse)


# Load data ---------------------------------------------------------------

# Play by play data
nba_pbp <- hoopR::load_nba_pbp(season = 2003:2023)
# Game outcomes / team level stats
nba_team <- hoopR::load_nba_team_box(season = 2003:2023) 
# Team Year Stats
nba_year <- nba_teamyearbyyearstats()
# Player stats 
nba_player <- hoopR::load_nba_player_box()

# Filter late game context ------------------------------------------------

endgame <- nba_pbp |> 
              filter(# only regular season or post-season games
                     season_type %in% c(1, 2), 
                     # 4th quarter/OTs
                     qtr %in% c(4:8), 
                     # last 24 seconds 
                     clock_minutes == 0, clock_seconds <= 24, clock_seconds > 0,
                     # score is within 3
                     abs(away_score - home_score) == 3)

# Did the team up by 3 foul? 

endgame_foul <- endgame |> 
                # create a new column indicating if team fouled
                mutate(home_team_foul = ifelse((team_id == home_team_id) &
                                                 # foul indicator
                                                  type_id == 45 & 
                                                 # if they were the team up when fouling
                                                 (home_score > away_score), 1, 0),
                       away_team_foul = ifelse((team_id == away_team_id) &
                                                 type_id == 45 & 
                                                 (away_score > home_score), 1, 0)) |> 
                # filter out all star game
                filter(away_team_abbrev != "LEB" & home_team_abbrev != "GIA")


# Clean dataset -----------------------------------------------------------

foul_games <- endgame_foul |> 
  filter(home_team_foul == 1 | away_team_foul == 1) |> 
  group_by(game_id) |> 
  # for the 10 games with multiple late-game fouls, keep the one closest to the end
  slice_min(order_by = clock_seconds) |> 
  # add indicator
  mutate(foul = 1)

# 306 games with a late-game foul

no_foul_games <- endgame_foul |> 
  filter(home_team_foul == 0 & away_team_foul == 0) |> 
  # just keep 1 possession 
  distinct(game_id, .keep_all = TRUE) |> 
  # add indicator
  mutate(foul = 0)

# 5122 games without

# Append the datasets
all_games <- bind_rows(foul_games, no_foul_games) |> 
            # filtering out unnecessary columns
            select(game_id, season, foul, home_team_foul, away_team_foul,
                   clock_seconds, qtr, home_score, away_score, 
                   team_id, home_team_id, away_team_id)

# Join game level details -------------------------------------------------------

# Find out winner of the games
outcome <- nba_team |> 
              filter(game_id %in% all_games$game_id,
                     team_winner == TRUE) |> 
              mutate(winner = team_home_away) |> 
              select(game_id, winner)
  
all_games <- all_games |> 
  mutate(leading_team = ifelse((home_score > away_score), "home", "away"))   

# Join outcome
all_games <- all_games |> 
              left_join(outcome, by = c("game_id")) 

# Determine if leading team who the game (success)
all_games <- all_games |> 
              mutate(success = ifelse((leading_team == winner),1,0))

# How many team held onto the lead?
#table(all_games$success, all_games$foul)

# Add columns with game level statistics
game_stats <- nba_team |> 
                filter(game_id %in% all_games$game_id) |> 
  group_by(game_id) |> 
  mutate(home_game_fgpct = ifelse(team_home_away == "home", field_goal_pct, NA),
         away_game_fgpct = ifelse(team_home_away == "away", field_goal_pct, NA),
         home_game_3pct = ifelse(team_home_away == "home", three_point_field_goal_pct, NA),
         away_game_3pct = ifelse(team_home_away == "away", three_point_field_goal_pct, NA),
         home_game_FTpct = ifelse(team_home_away == "home", free_throw_pct, NA),
         away_game_FTpct = ifelse(team_home_away == "away", free_throw_pct, NA)) |> 
  select(game_id, home_game_fgpct, away_game_fgpct, home_game_3pct, away_game_3pct,
         home_game_FTpct, away_game_FTpct) |> 
  ungroup()

game_stats <- game_stats |> 
  group_by(game_id) |> 
  summarize(across(starts_with("home_game_"), ~ first(na.omit(.))),
            across(starts_with("away_game_"), ~ first(na.omit(.))))

# Join stats
all_games <- all_games |> 
  left_join(game_stats, by = c("game_id")) |> 
  # Select columns
  select(game_id, season, foul, qtr, home_team_id,
         away_team_id, 
         13:21, clock_seconds)
  
# Replace ID with team abbreviation
teams <- endgame %>%
  distinct(home_team_id, home_team_abbrev) |> 
  rename(id = home_team_id, team_name = home_team_abbrev)

for (id in unique(all_games$home_team_id)) {
  team_name <- teams$team_name[teams$id == id]
  all_games$home_team_id[all_games$home_team_id == id] <- team_name
}
for (id in unique(all_games$away_team_id)) {
  team_name <- teams$team_name[teams$id == id]
  all_games$away_team_id[all_games$away_team_id == id] <- team_name
}

nba_foul_data <- all_games |> 
  mutate(leading_fgpct = ifelse(leading_team == "home", home_game_fgpct, away_game_fgpct),
         leading_3pct = ifelse(leading_team == "home", home_game_3pct, away_game_3pct),
         leading_FTpct = ifelse(leading_team == "home", home_game_FTpct, away_game_FTpct),
         opponent_fgpct = ifelse(leading_team == "home", away_game_fgpct, home_game_fgpct),
         opponent_3pct = ifelse(leading_team == "home", away_game_3pct, home_game_3pct),
         opponent_FTpct = ifelse(leading_team == "home", away_game_FTpct, home_game_FTpct)) |> 
  na.omit()
  
# Create csv for cleaned dataset
#write.csv(all_games, "project/nba_foul_data.csv", row.names = FALSE)

# Join season level stats ----------------------------------

# Could join something like a season level team net rating, 
# but within game stats might be more predictive of success

# Join player stats -------------------------------------------------------

# THIS MIGHT HAVE TO BE FUTURE RESEARCH 
# GOING TO BE DIFFICULT TO ISOLATE THE PLAYERS THAT WERE ON THE FLOOR 




