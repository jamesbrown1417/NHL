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
  # Get Markets
  bet365_hockey_markets <-
    read_html(scraped_file) |> 
    html_nodes(".gl-MarketGroupPod")
  
  # Market Names
  market_names <-
    bet365_hockey_markets |> 
    html_elements(".cm-MarketGroupWithIconsButton_Text") |> 
    html_text()
  
  # Function to extract player data
  extract_player_data <- function(market_name) {
    index <- which(market_names == market_name)
    
    players <-
      bet365_hockey_markets[[index]] |> 
      html_elements(".srb-ParticipantLabelWithTeam_Name") |> 
      html_text()
    
    cols <-
      bet365_hockey_markets[[index]] |> 
      html_elements(".gl-Market_General")
    
    over_index <- which(str_detect(cols |> html_text(), "Over"))
    under_index <- which(str_detect(cols |> html_text(), "Under"))
    
    over_odds <-
      cols[[over_index]] |> 
      html_elements(".gl-ParticipantCenteredStacked_Odds") |> 
      html_text()
    
    under_odds <-
      cols[[under_index]] |> 
      html_elements(".gl-ParticipantCenteredStacked_Odds") |> 
      html_text()
    
    tibble(
      player = players,
      over_price = as.numeric(over_odds),
      under_price = as.numeric(under_odds),
      market_name = market_name,
      agency = "Bet365"
    )
  }
  
  # Extract markets
  shots_on_goal <- extract_player_data("Shots on Goal O/U")
  points <- extract_player_data("Points O/U")
  assists <- extract_player_data("Assists O/U")
  anytime_goal_scorer <- extract_player_data("Anytime Goal Scorer") |> mutate(line = 1)
  
  # Get teams
  team_names <-
    scraped_file |> 
    read_html() |> 
    html_nodes(".sph-FixturePodHeader_TeamName") |> 
    html_text()
  
  match_name <- paste(team_names, collapse = " v ")
  
  # Add match name to each dataset
  shots_on_goal <- shots_on_goal |> mutate(match = match_name)
  points <- points |> mutate(match = match_name)
  assists <- assists |> mutate(match = match_name)
  anytime_goal_scorer <- anytime_goal_scorer |> mutate(match = match_name)
  
  return(list(
    shots_on_goal = shots_on_goal,
    points = points,
    assists = assists,
    anytime_goal_scorer = anytime_goal_scorer
  ))
}

# Create safe version of function
get_player_props_safe <- safely(get_player_props)

# Map over all html files
list_of_player_props <- map(scraped_files_player, get_player_props_safe)

# Keep only successful results
list_of_player_props <- list_of_player_props |> keep(~is.null(.x$error)) |> map("result")

# Split into separate datasets
player_shots_on_goal <- map_dfr(list_of_player_props, "shots_on_goal")
player_points <- map_dfr(list_of_player_props, "points")
player_assists <- map_dfr(list_of_player_props, "assists")
player_anytime_goal_scorer <- map_dfr(list_of_player_props, "anytime_goal_scorer")

# Write out
write_csv(player_shots_on_goal, "Data/scraped_odds/bet365_hockey_shots_on_goal.csv")
write_csv(player_points, "Data/scraped_odds/bet365_hockey_points.csv")
write_csv(player_assists, "Data/scraped_odds/bet365_hockey_assists.csv")
write_csv(player_anytime_goal_scorer, "Data/scraped_odds/bet365_hockey_anytime_goal_scorer.csv")
