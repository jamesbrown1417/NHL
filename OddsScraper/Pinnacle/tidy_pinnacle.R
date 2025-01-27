# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Read in data
all_pinnacle_raw_files <- list.files("OddsScraper/Pinnacle", "\\.csv", full.names = TRUE)

all_pinnacle_data <-
  map(all_pinnacle_raw_files, read_csv) |> 
  bind_rows()

#===============================================================================
# Player Shots on Goal
#===============================================================================

# Get all shots on goal Markets
shots_on_goal_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Shots"))

# Shots on Goal - over
shots_on_goal_over <-
  shots_on_goal_markets |> 
  mutate(player_name = str_remove(selection, " \\(Shots on Goal\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Over")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Shots on Goal") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    over_price = price,
    agency
  )

# Shots on Goal - under
shots_on_goal_under <-
  shots_on_goal_markets |> 
  mutate(player_name = str_remove(selection, " \\(Shots on Goal\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Under")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Shots on Goal") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    under_price = price,
    agency
  )

# Combine shots on goal data
player_shots_on_goal_data <-
  inner_join(shots_on_goal_over, shots_on_goal_under) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  rename(market_name = market)

# Write to CSV
write_csv(player_shots_on_goal_data, "Data/scraped_odds/pinnacle_shots_on_goal.csv")

#===============================================================================
# Player Assists
#===============================================================================

# Get all assists Markets
assists_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Assists"))

# Assists - over
assists_over <-
  assists_markets |> 
  mutate(player_name = str_remove(selection, " \\(Assists\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Over")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Assists") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    over_price = price,
    agency
  )

# Assists - under
assists_under <-
  assists_markets |> 
  mutate(player_name = str_remove(selection, " \\(Assists\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Under")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Assists") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    under_price = price,
    agency
  )

# Combine assists data
player_assists_data <-
  inner_join(assists_over, assists_under) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  rename(market_name = market)

# Write to CSV
write_csv(player_assists_data, "Data/scraped_odds/pinnacle_assists.csv")

#===============================================================================
# Player Points
#===============================================================================

# Get all points Markets
points_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Points"))

# Points - over
points_over <-
  points_markets |> 
  mutate(player_name = str_remove(selection, " \\(Points\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Over")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Points") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    over_price = price,
    agency
  )

# Points - under
points_under <-
  points_markets |> 
  mutate(player_name = str_remove(selection, " \\(Points\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Under")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Points") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    under_price = price,
    agency
  )

# Combine points data
player_points_data <-
  inner_join(points_over, points_under) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  rename(market_name = market)

# Write to CSV
write_csv(player_points_data, "Data/scraped_odds/pinnacle_points.csv")

#===============================================================================
# Player Goals
#===============================================================================

# Get all goals Markets
goals_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Goals"))

# Goals - over
goals_over <-
  goals_markets |> 
  mutate(player_name = str_remove(selection, " \\(Goals\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Over")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Goals") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    over_price = price,
    agency
  )

# Goals - under
goals_under <-
  goals_markets |> 
  mutate(player_name = str_remove(selection, " \\(Goals\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Under")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Goals") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    under_price = price,
    agency
  )

# Combine goals data
player_goals_data <-
  inner_join(goals_over, goals_under) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  rename(market_name = market)

# Write to CSV
write_csv(player_goals_data, "Data/scraped_odds/pinnacle_goals.csv")

#===============================================================================
# Final Output Summary
#===============================================================================

cat("Data processing complete. The following CSVs have been created:\n",
    "- pinnacle_shots_on_goal.csv\n",
    "- pinnacle_assists.csv\n",
    "- pinnacle_points.csv\n",
    "- pinnacle_goals.csv\n")