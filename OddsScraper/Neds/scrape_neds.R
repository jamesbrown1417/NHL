# Libraries
library(tidyverse)
library(jsonlite)

# Read match URLs
df <- read_csv("OddsScraper/Neds/neds_nhl_match_urls.csv")

# Get match JSON files
json_match_files <- list.files("OddsScraper/Neds/", pattern = "^data_.*.json", full.names = TRUE)

event_json_list <- map(json_match_files, ~fromJSON(.x))

#===============================================================================
# Extract Market Data
#===============================================================================

# Initialize empty vectors to store extracted data
event_ids <- character()
entrants <- character()
market_id <- character()
match_names <- character()
handicaps <- numeric()
prices <- numeric()
market_lookup_name <- character()
market_lookup_id <- character()

# Loop through the entrants
for (i in seq_along(event_json_list)) {
  match <- event_json_list[[i]]
  
  for (entrant in match$entrants) {
    entrants <- c(entrants, entrant$name)
    market_id <- c(market_id, entrant$market_id)
    event_ids <- c(event_ids, match$events[[1]]$id)
  }
  
  # Extract market information
  for (market in match$markets) {
    market_lookup_name <- c(market_lookup_name, market$name)
    market_lookup_id <- c(market_lookup_id, market$id)
    
    if (is.null(market$handicap)) {
      handicaps <- c(handicaps, NA)
    } else {
      handicaps <- c(handicaps, market$handicap)
    }
  }
  
  # Extract odds/prices
  for (price in match$prices) {
    fractional_odds <- price$odds$numerator / price$odds$denominator
    decimal_odds <- fractional_odds + 1
    prices <- c(prices, decimal_odds)
  }
}

# Create market lookup dataframe
market_lookup_df <- data.frame(market_id = market_lookup_id, market_name = market_lookup_name, handicaps = handicaps)

# Create market dataframe
market_df <- data.frame(event_id = event_ids, market_id = market_id, entrants = entrants, price = prices)

# Merge market lookup dataframe with market dataframe
market_df <- merge(market_df, market_lookup_df, by = 'market_id', all.x = TRUE)

# Reorder columns
market_df <- market_df |> select(event_id, market_name, entrants, handicaps, price)

# Add match names
market_df <-
  market_df |> 
  left_join(df[,c("event_name", "event_id")], by = c("event_id" = "event_id")) |> 
  relocate(event_name, .before = event_id) |> 
  rename(match_name = event_name) |> 
  select(-event_id)

#===============================================================================
# Player Shots on Goal
#===============================================================================

# Filter to only include player shots on goal markets
player_shots_data <-
  market_df |> 
  filter(str_detect(market_name, "Player Shots"))

# Overs
shots_overs <-
  player_shots_data |>
  filter(str_detect(entrants, "Over")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+\\.\\d+"))) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(market_name, pattern <- ".*(?= \\()")) |>
  mutate(player_name_1 = str_remove(player_name_1, "Player Shots O/U - ")) |>
  mutate(player_name_1 = str_remove_all(player_name_1, " \\(\\d+\\.\\d+\\)")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= - ).*")) |> 
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = str_replace(match_name, " vs ", " v "),
    market_name = "Player Shots on Goal",
    player_name,
    line = handicap,
    over_price = price,
    agency = "Neds"
  )

# Unders
shots_unders <-
  player_shots_data |>
  filter(str_detect(entrants, "Under")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+\\.\\d+"))) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(market_name, pattern <- ".*(?= \\()")) |>
  mutate(player_name_1 = str_remove(player_name_1, "Player Shots O/U - ")) |>
  mutate(player_name_1 = str_remove_all(player_name_1, " \\(\\d+\\.\\d+\\)")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= - ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = str_replace(match_name, " vs ", " v "),
    market_name = "Player Shots on Goal",
    player_name,
    line = handicap,
    under_price = price,
    agency = "Neds"
  )

# Merge overs and unders
player_shots_data <-
  shots_overs |> 
  full_join(shots_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
  select(match, market_name, player_name, line, over_price, under_price, agency) |> 
  arrange(match, player_name, line)

# Write to CSV
player_shots_data |> write_csv("Data/scraped_odds/neds_player_shots_on_goal.csv")

#===============================================================================
# Player Assists
#===============================================================================

# Filter to only include player assists markets
player_assists_data <-
  market_df |> 
  filter(str_detect(market_name, "Player Assists"))

# Overs
assists_overs <-
  player_assists_data |>
  filter(str_detect(entrants, "Over")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+\\.\\d+"))) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_remove(market_name, "Player Assists O/U - ")) |>
  mutate(player_name_1 = str_remove_all(player_name_1, " \\(\\d+\\.\\d+\\)")) |>
  transmute(
    match = str_replace(match_name, " vs ", " v "),
    market_name = "Player Assists",
    player_name = player_name_1,
    line = handicap,
    over_price = price,
    agency = "Neds"
  )

# Unders
assists_unders <-
  player_assists_data |>
  filter(str_detect(entrants, "Under")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+\\.\\d+"))) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_remove(market_name, "Player Assists O/U - ")) |>
  mutate(player_name_1 = str_remove_all(player_name_1, " \\(\\d+\\.\\d+\\)")) |>
  transmute(
    match = str_replace(match_name, " vs ", " v "),
    market_name = "Player Assists",
    player_name = player_name_1,
    line = handicap,
    under_price = price,
    agency = "Neds"
  )

# Merge overs and unders
player_assists_data <-
  assists_overs |> 
  full_join(assists_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
  select(match, market_name, player_name, line, over_price, under_price, agency) |> 
  arrange(match, player_name, line)

# Write to CSV
player_assists_data |> write_csv("Data/scraped_odds/neds_player_assists.csv")

#===============================================================================
# Player Points
#===============================================================================

# Filter to only include player points markets
player_points_data <-
  market_df |> 
  filter(str_detect(market_name, "Player Points"))

# Overs
points_overs <-
  player_points_data |>
  filter(str_detect(entrants, "Over")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+\\.\\d+"))) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_remove(market_name, "Player Points O/U - ")) |>
  mutate(player_name_1 = str_remove_all(player_name_1, " \\(\\d+\\.\\d+\\)")) |>
  transmute(
    match = str_replace(match_name, " vs ", " v "),
    market_name = "Player Points",
    player_name = player_name_1,
    line = handicap,
    over_price = price,
    agency = "Neds"
  )

# Unders
points_unders <-
  player_points_data |>
  filter(str_detect(entrants, "Under")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+\\.\\d+"))) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_remove(market_name, "Player Points O/U - ")) |>
  mutate(player_name_1 = str_remove_all(player_name_1, " \\(\\d+\\.\\d+\\)")) |>
  transmute(
    match = str_replace(match_name, " vs ", " v "),
    market_name = "Player Points",
    player_name = player_name_1,
    line = handicap,
    under_price = price,
    agency = "Neds"
  )

# Merge overs and unders
player_points_data <-
  points_overs |> 
  full_join(points_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
  select(match, market_name, player_name, line, over_price, under_price, agency) |> 
  arrange(match, player_name, line)

# Write to CSV
player_points_data |> write_csv("Data/scraped_odds/neds_player_points.csv")
