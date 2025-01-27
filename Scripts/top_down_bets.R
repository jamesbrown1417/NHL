# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# read in function for fixing team names
source("Scripts/fix_team_names.R")

#===============================================================================
# Player shots on goal
#===============================================================================

# Read in all player shots on goal data-----------------------------------------
odds_files <- list.files("Data/scraped_odds", full.names = TRUE, pattern = "shots") |> 
  map(~ read_csv(.x))

# Get all player shots on goal data
all_player_shots_on_goal <-
  bind_rows(odds_files) |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_nhl_team_names(home_team),
         away_team = fix_nhl_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Get all overs
all_overs <-
  all_player_shots_on_goal |>
  filter(!is.na(over_price)) |> 
  select(-under_price) |> 
  distinct(player_name, line, over_price, .keep_all = TRUE) |> 
  rename(over_agency = agency)

# Get all unders
all_unders <-
  all_player_shots_on_goal |>
  filter(!is.na(under_price)) |> 
  select(-over_price) |> 
  distinct(player_name, line, under_price, .keep_all = TRUE) |> 
  rename(under_agency = agency)

# Combine
shots_final <-
all_overs |> 
  inner_join(all_unders) |>
  relocate(under_price, under_agency, .after = over_price) |>
  relocate(over_agency, .after = over_price) |> 
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin)

#===============================================================================
# Player assists
#===============================================================================

# Read in all player assists data----------------------------------------------
odds_files_assists <- list.files("Data/scraped_odds", full.names = TRUE, pattern = "assists") |> 
  map(~ read_csv(.x))

# Get rid of anything with length 0 rows
odds_files_assists <- odds_files_assists |> 
  map(~ if (nrow(.x) > 0) .x else NULL) |> 
  compact()

# Get all player assists data
all_player_assists <-
  bind_rows(odds_files_assists) |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_nhl_team_names(home_team),
         away_team = fix_nhl_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Get all overs
all_overs_assists <-
  all_player_assists |>
  filter(!is.na(over_price)) |> 
  select(-under_price) |> 
  distinct(player_name, line, over_price, .keep_all = TRUE) |> 
  rename(over_agency = agency)

# Get all unders
all_unders_assists <-
  all_player_assists |>
  filter(!is.na(under_price)) |> 
  select(-over_price) |> 
  distinct(player_name, line, under_price, .keep_all = TRUE) |> 
  rename(under_agency = agency)

# Combine
assists_final <-
  all_overs_assists |> 
  inner_join(all_unders_assists) |>
  relocate(under_price, under_agency, .after = over_price) |>
  relocate(over_agency, .after = over_price) |> 
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin) * 100)

#===============================================================================
# Player points
#===============================================================================

# Read in all player points data-----------------------------------------------
odds_files_points <- list.files("Data/scraped_odds", full.names = TRUE, pattern = "points") |> 
  map(~ read_csv(.x))

# Get rid of anything with length 0 rows
odds_files_points <- odds_files_points |> 
  map(~ if (nrow(.x) > 0) .x else NULL) |> 
  compact()

# Get all player points data
all_player_points <-
  bind_rows(odds_files_points) |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_nhl_team_names(home_team),
         away_team = fix_nhl_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Get all overs
all_overs_points <-
  all_player_points |>
  filter(!is.na(over_price)) |> 
  select(-under_price) |> 
  distinct(player_name, line, over_price, .keep_all = TRUE) |> 
  rename(over_agency = agency)

# Get all unders
all_unders_points <-
  all_player_points |>
  filter(!is.na(under_price)) |> 
  select(-over_price) |> 
  distinct(player_name, line, under_price, .keep_all = TRUE) |> 
  rename(under_agency = agency)

# Combine
points_final <-
  all_overs_points |> 
  inner_join(all_unders_points) |>
  relocate(under_price, under_agency, .after = over_price) |>
  relocate(over_agency, .after = over_price) |> 
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin) * 100)

# Write to file----------------------------------------------------------------
write_csv(shots_final, "Data/processed_odds/shots_final.csv")
write_csv(assists_final, "Data/processed_odds/assists_final.csv")
write_csv(points_final, "Data/processed_odds/points_final.csv")