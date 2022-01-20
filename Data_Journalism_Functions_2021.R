## Clean code for DJ project
## Set-ups
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(graphics)

data_year <- function(year){
  ## Variables
  data <- load_pbp(year)
  
  ## Organize data
  pbp_rp <- data %>%
    filter(rush == 1 | pass == 1, !is.na(epa))
  
  pbp_rp
}

##Data for each year
data_2016 <- data_year(2016)
data_2017 <- data_year(2017)
data_2018 <- data_year(2018)
data_2019 <- data_year(2019)
data_2020 <- data_year(2020)

get_wins <- function(year){
  ## Get Wins
  games <- nflreadr::load_schedules()
  home <- games %>%
    filter(game_type == 'REG') %>%
    select(season, week, home_team, result) %>%
    rename(team = home_team)
  away <- games %>%
    filter(game_type == 'REG') %>%
    select(season, week, away_team, result) %>%
    rename(team = away_team) %>%
    mutate(result = -result)
  results <- bind_rows(home, away) %>%
    arrange(week) %>%
    mutate(
      win = case_when(
        result > 0 ~ 1,
        result < 0 ~ 0,
        result == 0 ~ 0.5
      )
    )
  team_wins <- results %>%
    group_by(team, season) %>%
    summarize(
      wins = sum(win),
      point_diff = sum(result)) %>%
    ungroup()
  wins_year <- team_wins %>%
    arrange(-wins) %>%
    filter(season == year)
  win_pct <- (wins_year[, 3] / 16) * 100
  win_pct <- unlist(win_pct)
  teams <- wins_year[, 1]
  teams <- unlist(teams)
  list(win_pct, teams)
}

##Win percentage[[1]] and team names[[2]] for each year
vals_2016 <- get_wins(2016)
vals_2017 <- get_wins(2017)
vals_2018 <- get_wins(2018)
vals_2019 <- get_wins(2019)
vals_2020 <- get_wins(2020)

fix_team <- function(val){
  val[[2]][which(val[[2]] == "OAK")] = "LV"
  val[[2]][which(val[[2]] == "SD")] = "LAC"
  val[[2]]
}

## Get WR Data function
get_wr_data <- function(data, vals, wr_num){
  all_vals <- NULL
  teams <- fix_team(vals)
  for (i in 1:32){
    team_year <- data %>%
      filter(pass == 1, posteam == teams[i], !is.na(receiver)) %>%
      group_by(receiver) %>%
      summarize(
        mean_epa = mean(epa), success_rate = mean(success), ypc = mean(yards_gained), plays = n()
      ) %>%
      arrange(-plays)
    team_wr_pct <- (team_year[wr_num, 5] / sum(team_year[, 5])) * 100
    all_vals <- c(all_vals, team_wr_pct)
  }
  all_vals
}

##2016
wr1_data_2016 <- unlist(get_wr_data(data_2016, vals_2016, 1))
wr2_data_2016 <- unlist(get_wr_data(data_2016, vals_2016, 2))
wr3_data_2016 <- unlist(get_wr_data(data_2016, vals_2016, 3))
mat_2016 <- rbind(wr1_data_2016, wr2_data_2016, wr3_data_2016)

##2017
wr1_data_2017 <- unlist(get_wr_data(data_2017, vals_2017, 1))
wr2_data_2017 <- unlist(get_wr_data(data_2017, vals_2017, 2))
wr3_data_2017 <- unlist(get_wr_data(data_2017, vals_2017, 3))
mat_2017 <- rbind(wr1_data_2017, wr2_data_2017, wr3_data_2017)

##2018
wr1_data_2018 <- unlist(get_wr_data(data_2018, vals_2018, 1))
wr2_data_2018 <- unlist(get_wr_data(data_2018, vals_2018, 2))
wr3_data_2018 <- unlist(get_wr_data(data_2018, vals_2018, 3))

##2019
wr1_data_2019 <- unlist(get_wr_data(data_2019, vals_2019, 1))
wr2_data_2019 <- unlist(get_wr_data(data_2019, vals_2019, 2))
wr3_data_2019 <- unlist(get_wr_data(data_2019, vals_2019, 3))

##2020
wr1_data_2020 <- unlist(get_wr_data(data_2020, vals_2020, 1))
wr2_data_2020 <- unlist(get_wr_data(data_2020, vals_2020, 2))
wr3_data_2020 <- unlist(get_wr_data(data_2020, vals_2020, 3))

##Data Totals
wr1_data <- c(wr1_data_2016, wr1_data_2017, wr1_data_2018, wr1_data_2019, wr1_data_2020)
wr2_data <- c(wr2_data_2016, wr2_data_2017, wr2_data_2018, wr2_data_2019, wr2_data_2020)
wr3_data <- c(wr3_data_2016, wr3_data_2017, wr3_data_2018, wr3_data_2019, wr3_data_2020)
win_pct <- c(vals_2016[[1]], vals_2017[[1]], vals_2018[[1]], vals_2019[[1]], vals_2020[[1]])

## Plotting Data Function
get_plot <- function(vals, wr_data, title){
  wr_data <- as.numeric(wr_data)
  if (class(vals) == "list"){
    pct <- as.numeric(vals[[1]])
  }
  else{
    pct <- vals
  }
  linear_model <- lm(wr_data ~ pct)
  plot(wr_data ~ pct, main = title, xlab="Win Pct", ylab="WR Target Pct") 
  abline(mean(wr_data, na.rm=T), 0, col="blue")
  abline(linear_model) 
  legend("topleft", legend = c("LM Trend", "NFL Average"), col = c("black", "blue"),
         lty = c(1, 1), lwd = c(1, 1), inset = 0.05, cex = 0.35)
}

##Plotting Totals
get_plot(win_pct, wr1_data, "WR1 Target Pct vs Win Pct")
get_plot(win_pct, wr2_data, "WR2 Target Pct vs Win Pct")
get_plot(win_pct, wr3_data, "WR3 Target Pct vs Win Pct")

##tot_mat contains data for wr1, wr2 and wr3
tot_mat <- rbind(wr1_data, wr2_data, wr3_data)

##two contains data for wr1 and wr2
two <- rbind(wr1_data, wr2_data)

##Plotting both tot_mat and two
get_plot(win_pct, apply(tot_mat, 2, sum), "WR Target Pct vs Win Pct")
get_plot(win_pct, apply(two, 2, sum), "WR Target Pct vs Win Pct")

##Linear models for data - 2nd value is the slope
lm(wr1_data ~ win_pct)
lm(wr2_data ~ win_pct)
lm(wr3_data ~ win_pct)
lm(apply(tot_mat, 2, sum) ~ win_pct)
lm(apply(two, 2, sum) ~ win_pct)