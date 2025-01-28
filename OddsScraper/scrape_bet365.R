# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Read scraped HTML from the BET365_HTML Folder
scraped_files_player <- list.files("Data/BET365_HTML/", full.names = TRUE, pattern = "hockey")

# Main Function
get_player_props <- function(scraped_file) {
  
  # Extract match details
  match_teams <- read_html(scraped_file) |>
    html_elements(".sph-FixturePodHeader_TeamName") |>
    html_text() |> 
    str_trim()
  
  # Format the match string as "HomeTeam v AwayTeam"
  match_name <- glue("{match_teams[2]} v {match_teams[1]}")
  
  # Get Markets
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  # Market Names
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
    html_text()
  
  #=============================================================================
  # Player Shots on Goal Alternate Lines
  #=============================================================================
  
  shots_on_goal_alt_index <- which(market_names == "Shots on Goal")
  
  # Extract shot lines (1, 2, 3, ...)
  shots_on_goal_alt_lines <- 
    bet365_player_markets[[shots_on_goal_alt_index]] |>
    html_elements(".srb-HScrollPlaceHeader") |>
    html_text() |> 
    as.numeric()
  
  # Extract player names
  shots_on_goal_alt_players <- 
    bet365_player_markets[[shots_on_goal_alt_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # Extract odds for each line separately
  shots_on_goal_alt_odds <- 
    bet365_player_markets[[shots_on_goal_alt_index]] |>
    html_elements(".srb-HScrollPlaceColumnMarket") |>
    map(~ tibble(
      player = shots_on_goal_alt_players,
      line = as.numeric(html_element(.x, ".srb-HScrollPlaceHeader") |> html_text()) - 0.5,
      over_price = html_elements(.x, ".gl-ParticipantOddsOnly_Odds") |> html_text() |> as.numeric()
    ))
  
  # Combine all odds data into a single tibble
  player_shots_on_goal_alt <- 
    bind_rows(shots_on_goal_alt_odds) |>
    mutate(market_name = "Player Shots on Goal Alternate Lines",
           agency = "Bet365")
  
  #=============================================================================
  # Player Shots on Goal Over / Under
  #=============================================================================
  
  shots_on_goal_over_under_index <- which(market_names == "Shots on Goal O/U")
  
  shots_on_goal_players <-
    bet365_player_markets[[shots_on_goal_over_under_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  shots_on_goal_cols <-
    bet365_player_markets[[shots_on_goal_over_under_index]] |>
    html_elements(".gl-Market_General")
  
  shots_on_goal_over_index <- which(str_detect(shots_on_goal_cols |> html_text(), "Over"))
  
  shots_on_goal_over_lines <-
    shots_on_goal_cols[[shots_on_goal_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  shots_on_goal_over_odds <-
    shots_on_goal_cols[[shots_on_goal_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  shots_on_goal_under_index <- which(str_detect(shots_on_goal_cols |> html_text(), "Under"))
  shots_on_goal_under_odds <-
    shots_on_goal_cols[[shots_on_goal_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  player_shots_on_goal <-
    tibble(player = shots_on_goal_players,
           line = as.numeric(shots_on_goal_over_lines),
           over_price = as.numeric(shots_on_goal_over_odds),
           under_price = as.numeric(shots_on_goal_under_odds)) |>
    mutate(market_name = "Player Shots on Goal Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Player Assists Alternate Lines
  #=============================================================================
  
  assists_alt_index <- which(market_names == "Assists")
  
  assists_alt_players <- 
    bet365_player_markets[[assists_alt_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  assists_alt_lines <- 
    bet365_player_markets[[assists_alt_index]] |>
    html_elements(".srb-HScrollPlaceHeader") |>
    html_text() |> 
    as.numeric()
  
  assists_alt_odds <- 
    bet365_player_markets[[assists_alt_index]] |>
    html_elements(".srb-HScrollPlaceColumnMarket") |>
    map(~ tibble(
      player = assists_alt_players,
      line = as.numeric(html_element(.x, ".srb-HScrollPlaceHeader") |> html_text()) - 0.5,
      over_price = html_elements(.x, ".gl-ParticipantOddsOnly_Odds") |> html_text() |> as.numeric()
    ))
  
  player_assists_alt <- 
    bind_rows(assists_alt_odds) |>
    mutate(market_name = "Player Assists Alternate Lines",
           agency = "Bet365")
  
  #=============================================================================
  # Player Assists Over / Under
  #=============================================================================
  
  assists_over_under_index <- which(market_names == "Assists O/U")
  
  assists_players <- 
    bet365_player_markets[[assists_over_under_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  assists_cols <- 
    bet365_player_markets[[assists_over_under_index]] |>
    html_elements(".gl-Market_General")
  
  assists_over_index <- which(str_detect(assists_cols |> html_text(), "Over"))
  
  assists_over_lines <- 
    assists_cols[[assists_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  assists_over_odds <- 
    assists_cols[[assists_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  assists_under_index <- which(str_detect(assists_cols |> html_text(), "Under"))
  
  assists_under_odds <- 
    assists_cols[[assists_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  player_assists <- 
    tibble(player = assists_players,
           line = as.numeric(assists_over_lines),
           over_price = as.numeric(assists_over_odds),
           under_price = as.numeric(assists_under_odds)) |>
    mutate(market_name = "Player Assists Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Player Points Alternate Lines
  #=============================================================================
  
  points_alt_index <- which(market_names == "Points")
  
  points_alt_players <- 
    bet365_player_markets[[points_alt_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  points_alt_lines <- 
    bet365_player_markets[[points_alt_index]] |>
    html_elements(".srb-HScrollPlaceHeader") |>
    html_text() |> 
    as.numeric()
  
  points_alt_odds <- 
    bet365_player_markets[[points_alt_index]] |>
    html_elements(".srb-HScrollPlaceColumnMarket") |>
    map(~ tibble(
      player = points_alt_players,
      line = as.numeric(html_element(.x, ".srb-HScrollPlaceHeader") |> html_text()) - 0.5,
      over_price = html_elements(.x, ".gl-ParticipantOddsOnly_Odds") |> html_text() |> as.numeric()
    ))
  
  player_points_alt <- 
    bind_rows(points_alt_odds) |>
    mutate(market_name = "Player Points Alternate Lines",
           agency = "Bet365")
  
  #=============================================================================
  # Player Points Over / Under
  #=============================================================================
  
  points_over_under_index <- which(market_names == "Points O/U")
  
  points_players <- 
    bet365_player_markets[[points_over_under_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  points_cols <- 
    bet365_player_markets[[points_over_under_index]] |>
    html_elements(".gl-Market_General")
  
  points_over_index <- which(str_detect(points_cols |> html_text(), "Over"))
  
  points_over_lines <- 
    points_cols[[points_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  points_over_odds <- 
    points_cols[[points_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  points_under_index <- which(str_detect(points_cols |> html_text(), "Under"))
  
  points_under_odds <- 
    points_cols[[points_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  player_points <- 
    tibble(player = points_players,
           line = as.numeric(points_over_lines),
           over_price = as.numeric(points_over_odds),
           under_price = as.numeric(points_under_odds)) |>
    mutate(market_name = "Player Points Over/Under") |>
    mutate(agency = "Bet365")
  
  #===============================================================================
  # Combine all player markets
  #===============================================================================
  
  player_props_all <- 
    bind_rows(player_shots_on_goal_alt, player_shots_on_goal, 
              player_assists_alt, player_assists,
              player_points_alt, player_points) |> 
    arrange(player, line, over_price) |> 
    relocate(under_price, .after = over_price) |> 
    filter(!is.na(over_price) | !is.na(under_price)) |>
    mutate(match = match_name) |> 
    relocate(match, .before = player) |> 
    rename(player_name = player)
  
  return(player_props_all)
}

# Apply the function safely across all files
player_props_results <- map(scraped_files_player, safely(get_player_props))

# Extract results and errors separately
valid_results <- map(player_props_results, "result") |> compact()
failed_results <- map(player_props_results, "error") |> compact()

# Combine all valid results into a single data frame
player_props_combined <- bind_rows(valid_results)

#===============================================================================
# Save the data
#===============================================================================

# Player Points-----------------------------------------------------------------
player_props_combined |> 
  filter(market_name == "Player Points Over/Under" | market_name == "Player Points Alternate Lines") |>
  mutate(market_name = "Player Points") |>
  write_csv("Data/scraped_odds/bet365_player_points.csv")

# Player Shots on Goal----------------------------------------------------------
player_props_combined |> 
  filter(market_name == "Player Shots on Goal Over/Under" | market_name == "Player Shots on Goal Alternate Lines") |>
  mutate(market_name = "Player Shots on Goal") |>
  write_csv("Data/scraped_odds/bet365_player_shots_on_goal.csv")

# Player Assists----------------------------------------------------------------
player_props_combined |> 
  filter(market_name == "Player Assists Over/Under" | market_name == "Player Assists Alternate Lines") |>
  mutate(market_name = "Player Assists") |>
  write_csv("Data/scraped_odds/bet365_player_assists.csv")
