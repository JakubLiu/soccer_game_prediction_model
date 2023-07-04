# this is a model that predicts the outcome of a soccer game
# QBA LIU 2023 -------------------------------------------------------------------------------------------------------------

# data editing -------------------------------------------------------------------------------------------------------------
library(data.table)
premierleague <- fread("C:/Users/Lenovo/Desktop/STUDIA/SOCCER_GAMES_PREDICTION/premierleague.csv")
colnames(premierleague)[1] <- 'team'

average_goals_scored_in_league <- sum(premierleague$goals_for)/sum(premierleague$matches_played)
average_goals_scored_in_league

attack_rating <- rep(0, nrow(premierleague))
defense_rating <- rep(0, nrow(premierleague))
premierleague$attack_rating <- attack_rating
premierleague$defense_rating <- defense_rating

for (i in 1:nrow(premierleague)){
  premierleague$attack_rating[i] = (premierleague$goals_for[i]/premierleague$matches_played[i]) / average_goals_scored_in_league
  premierleague$defense_rating[i] = (premierleague$goals_against[i]/premierleague$matches_played[i]) / average_goals_scored_in_league
}

# the model ---------------------------------------------------------------------------------------------------------------------------
match_predictor <- function(home_team, away_team, league){
  home_idx <- which(league$team == home_team, arr.ind=TRUE)
  away_idx <- which(league$team == away_team, arr.ind=TRUE)
  
  home_expGoals <- league$attack_rating[home_idx] * league$defense_rating[away_idx] * average_goals_scored_in_league
  away_expGoals <- league$attack_rating[away_idx] * league$defense_rating[home_idx] * average_goals_scored_in_league
  
  possible_goals <- seq(0,15)
  col2 <- rep(0, length(possible_goals))
  col3 <- rep(0, length(possible_goals))
  col4 <- rep(0, length(possible_goals))
  col5 <- rep(0, length(possible_goals))
  pred_df <- data.frame(possible_goals, col2, col3, col4, col5)
  colnames(pred_df) <- c('possible_goals', home_team, away_team, paste0(home_team, "_win_chance"), paste0(away_team, "_win_chance"))
  
  for (i in 1:nrow(pred_df)){
    pred_df[i,2] <- dpois(x = pred_df$possible_goals[i], lambda = home_expGoals)
    pred_df[i,3] <- dpois(x = pred_df$possible_goals[i], lambda = away_expGoals)
  }
  for (j in 2:nrow(pred_df)){
    pred_df[j,4] <- pred_df[j,2] * sum(pred_df[1:j-1,3])
    pred_df[j,5] <- pred_df[j,3] * sum(pred_df[1:j-1,2])
  }
  
  column1 <- rep(0,1)
  column2 <- rep(0,1)
  column3 <- rep(0,1)
  result_df <- data.frame(column1, column2, column3)
  colnames(result_df) <- c(paste0(home_team, "_win_chance"), paste0(away_team, "_win_chance"), 'draw_chance')
  result_df[1,1] <- sum(pred_df[,4])
  result_df[1,2] <- sum(pred_df[,5])
  result_df[1,3] <- 1 - result_df[1,1] - result_df[1,2]
  result_df[1,1] <- label_percent()(result_df[1,1])
  result_df[1,2] <- label_percent()(result_df[1,2])
  result_df[1,3] <- label_percent()(result_df[1,3])
  
  list(details = pred_df, prediction = result_df)
}

# testing the model -------------------------------------------------------------------------------------------------------------------------------------------------
match_predictor(home_team = 'Man Utd', away_team = 'Arsenal', league = premierleague)