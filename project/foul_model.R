# Build model for 36460 final project

library(tidyverse)
library(brms)

nba_foul_data <- read_csv("project/nba_foul_data.csv")

#####################

# Figure out in-game stats for the leading/opponent teams

nba_foul_data <- nba_foul_data |> 
                    mutate(leading_fgpct = ifelse(leading_team == "home", home_game_fgpct, away_game_fgpct),
                           leading_3pct = ifelse(leading_team == "home", home_game_3pct, away_game_3pct),
                           leading_FTpct = ifelse(leading_team == "home", home_game_FTpct, away_game_FTpct),
                           opponent_fgpct = ifelse(leading_team == "home", away_game_fgpct, home_game_fgpct),
                           opponent_3pct = ifelse(leading_team == "home", away_game_3pct, home_game_3pct),
                           opponent_FTpct = ifelse(leading_team == "home", away_game_FTpct, home_game_FTpct)) |> 
                    na.omit()

#####################

foul_model <- brms::brm(success ~ foul + 
              # control for home-team advantage
              leading_team +
              # control for in-game stats 
               leading_fgpct + leading_3pct + leading_FTpct +
              # control for opponent stats interaction with fouls
              foul*opponent_fgpct + foul*opponent_3pct + foul*opponent_FTpct,
    family = bernoulli,
    data = nba_foul_data,
    seed = 11,
    sample_prior = T)

predict(foul_model, newdata = nba_foul_data, type = "response")[,1]

# CV

cv_rmse <- 
  map_dfr(unique(nba_foul_data$season),  
          function(holdout_season) {
            
            # Separate test and training data:
            test_data <- nba_foul_data |> 
              filter(season == holdout_season)
            train_data <- nba_foul_data |> 
              filter(season != holdout_season)
            
            # Train model:
            train_model <- brm(success ~ foul + leading_team +
                                      leading_fgpct + leading_3pct + leading_FTpct +
                                      foul*opponent_fgpct + foul*opponent_3pct + foul*opponent_FTpct,
                                    family = bernoulli,
                                    data = train_data,
                                    seed = 11,
                                    sample_prior = T)
            
            # Return tibble fo holdout results
            tibble(test_pred_probs = predict(train_model, newdata = test_data,
                                    type = "response")[,1],
                   test_actual = test_data$success,
                   test_season = holdout_season)
          })

# Confusion matrix
pred_success <- ifelse(cv_rmse$test_pred_probs < 0.5, 0, 1)
table("Predictions" = pred_success, "Observed" = nba_foul_data$success)

# Plot

nba_foul_data |> 
  mutate(pred_prob = cv_rmse$test_pred_probs) |> 
  ggplot(aes(x = pred_prob, y = nba_foul_data$success)) +
  #geom_point(aes(size = n_attempts)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, 
              color = "black", linetype = "dashed") +
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "Estimated success probability",
       y = "Observed success probability",
       title = "Calibration Plot") + 
  theme_bw() 

# Misclassification rate

cv_rmse |> 
  mutate(test_pred = ifelse(test_pred_probs < 0.5, 0, 1)) |> 
  summarize(mcr = mean(test_pred != test_actual))

# Brier score

cv_rmse |> 
  summarize(brier_score = mean((test_actual - test_pred_probs)^2))

# Holdout performance by season

cv_rmse |> 
  mutate(test_pred = ifelse(test_pred_probs < .5, 0, 1)) |> 
  group_by(test_season) |> 
  summarize(mcr = mean(test_pred != test_actual)) |> 
  ggplot(aes(x = test_season, y = mcr)) +
  geom_bar(stat = "identity", width = .1) + geom_point(size = 5) +
  labs(x = "Test Season",
       y = "Misclassification Rate",
       title = "Holdout performance by season") + 
  theme_bw() +
  scale_x_continuous(breaks = unique(cv_rmse$test_season))
  






# Extras - DID NOT USE!
brm_mod1 <- brms::brm(success ~ foul | trials(1) ~ 1 + 
                        # control for in-game stats for the leading team
                        leading_fgpct + leading_3pct + leading_FTpct, 
                      family = binomial, 
                      prior = c(prior(normal(0.2, 0.2), class = Intercept),
                                prior(normal(0, 1), coef = leading_fgpct),
                                prior(normal(0, 1), coef = leading_3pct),
                                prior(normal(0, 1), coef = leading_FTpct)),
                      iter = 2000, 
                      warmup = 1000,
                      chains = 4,
                      cores = 4,
                      seed = 4,
                      data = nba_foul_data)

b11.1 <-
  brm(data = nba_foul_data, 
      family = binomial,
      foul | trials(1) ~ 0,
      #prior(normal(0, 1), class = Intercept),
      seed = 11,
      sample_prior = T)



