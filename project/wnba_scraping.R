# Scrape data to use for project

library(wehoop)
library(tidyverse)

# Load data ---------------------------------------------------------------

# Play by play data
wnba_pbp <- wehoop::load_wnba_pbp(seasons = 2006:2023)	
# NOTE: starting in 2006 season, shot clock went from 30 to 24 seconds, 
# switched from two 20-minute halves to four 10-minute quarters

# Game outcomes / team level stats
wnba_team <- wehoop::load_wnba_team_box	
# Player stats 
wnba_player <- wehoop::load_wnba_player_box	

# Filter late game context ------------------------------------------------

endgame <- wnba_pbp |> 
  filter(qtr > 4, 
         # Last 24 seconds of 4th quarter/OTs
         clock_minutes == 0, clock_seconds <= 24,
         # Score is within 3
         abs(away_score - home_score) == 3)

# Did the team up by 3 foul? 

endgame_foul <- endgame |> 
  # create a new column indicating if team fouled
  mutate(home_team_foul = ifelse((team_id == home_team_id) &
                                   # Foul indicator
                                   type_id == 519 & 
                                   # If they were the team up when fouling
                                   (home_score > away_score), 1, 0),
         away_team_foul = ifelse((team_id == away_team_id) &
                                   type_id == 519 & 
                                   (away_score > home_score), 1, 0)) 

fouls <- endgame_foul |>   
  # group by game_id 
  group_by(game_id) |> 
  summarize(home_team_foul = sum(home_team_foul),
            away_team_foul = sum(away_team_foul))


# Out of 248 eligible scenarios, only 6 teams decided to foul 




