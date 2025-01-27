# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/ice-hockey-us"

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {

# Get data from main market page
matches <-
    sportsbet_url |> 
    read_html_live() |>
    html_nodes(".White_fqa53j6")
    
# Function to get team names
get_team_names <- function(match) {
    team_names <-
        match |>
        html_nodes(".size12_fq5j3k2") |>
        html_text()
    
    # Home team and Away Team
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    # Output
    tibble(home_team, away_team)
}

# Function to get odds
get_odds <- function(match) {
    odds <-
        match |>
        html_nodes(".priceTextSize_frw9zm9") |>
        html_text() |>
        as.numeric()
    
    # Home team
    home_win <- odds[2]
    away_win <- odds[1]
    
    # Output
    tibble(home_win, away_win)
}

# Function to get start time
get_start_time <- function(match) {
    start_time <-
        match |>
        html_nodes(".oneLine_f15ay66x") |>
        html_text()
    
    # Output
    tibble(start_time)
}

# Map functions to each match and combine together
all_main_market_data <-
bind_cols(
    map(matches, get_team_names) |> bind_rows() |> filter(!is.na(home_team)),
    map(matches, get_odds) |> bind_rows() |> filter(!is.na(home_win)),
    map(matches, get_start_time) |> bind_rows() |> filter(!is.na(start_time))
)

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

sportsbet_h2h <-
all_main_market_data |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           market_name,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Sportsbet")

# Write to csv
write_csv(sportsbet_h2h, "Data/scraped_odds/sportsbet_h2h.csv")

}

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

player_props_function <- function() {

# Function to get team names
get_team_names <- function(match) {
    team_names <-
        match |>
        html_nodes(".size12_fq5j3k2") |>
        html_text()
    
    # Home team and Away Team
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    # Output
    tibble(home_team, away_team)
}
    
    
# Get match links
match_links <-
sportsbet_url |> 
    read_html_live() |>
    html_nodes(".link_ft4u1lp") |> 
    html_attr("href")

# Get match IDs from links
match_ids <-
match_links |>
    str_extract("\\d{4,10}$") |>
    as.numeric()

# Get data from main market page
matches <-
    sportsbet_url |> 
    read_html_live() |>
    html_nodes(".White_fqa53j6")

# Get team names that correspond to each match link
team_names <-
    map_dfr(matches, get_team_names) |> filter(!is.na(home_team)) |> 
    bind_cols("match_id" = match_ids)

# Match info links
match_info_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/SportCard?displayWinnersPriceMkt=true&includeLiveMarketGroupings=true&includeCollection=true")

# Player goals links
player_goals_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/813/Markets")

# Player points links
player_points_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/814/Markets")

# Player shots on goal links
player_shots_on_goal_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/816/Markets")

# Player assists links
player_assists_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/817/Markets")

# Get IDs needed for SGM engine-------------------------------------------------
read_prop_url_metadata <- function(url) {
    
    # Make request and get response
  sb_response <-
    request(url) |>
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |> 
    req_headers("Referer" = "https://www.sportsbet.com.au") |>
    req_perform() |> 
    resp_body_json()
    
    # Empty vectors to append to
    class_external_id = c()
    competition_external_id = c()
    event_external_id = c()

    # Append to vectors
    class_external_id = c(class_external_id, sb_response$classExternalId)
    competition_external_id = c(competition_external_id, sb_response$competitionExternalId)
    event_external_id = c(event_external_id, sb_response$externalId)
    
    # Output
    tibble(class_external_id,
           competition_external_id,
           event_external_id,
           url) |> 
        mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
        rename(match_id = url) |> 
        mutate(match_id = as.numeric(match_id))
}

# Safe version that just returns NULL if there is an error
safe_read_prop_metadata <- safely(read_prop_url_metadata, otherwise = NULL)

# Map function to player points urls
player_prop_metadata <-
    map(match_info_links, safe_read_prop_metadata)

# Get just result part from output
player_prop_metadata <-
    player_prop_metadata |>
    map("result") |>
    map_df(bind_rows)

# Function to read a url and get the player props-------------------------------

read_prop_url <- function(url) {
    
    # Make request and get response
    sb_response <-
        request(url) |>
        req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |> 
        req_headers("Referer" = "https://www.sportsbet.com.au") |>
        req_perform() |> 
        resp_body_json()
    
    # Empty vectors to append to
    prop_market_name = c()
    selection_name_prop = c()
    prop_market_selection = c()
    prop_market_price = c()
    player_id = c()
    market_id = c()
    handicap = c()
    
    # Loop through each market
    for (market in sb_response) {
        for (selection in market$selections) {
            
            # Append to vectors
            prop_market_name = c(prop_market_name, market$name)
            selection_name_prop = c(selection_name_prop, selection$name)
            prop_market_selection = c(prop_market_selection, selection$resultType)
            prop_market_price = c(prop_market_price, selection$price$winPrice)
            player_id = c(player_id, selection$externalId)
            market_id = c(market_id, market$externalId)
            if (is.null(selection$unformattedHandicap)) {
                selection$unformattedHandicap = NA
                handicap = c(handicap, selection$unformattedHandicap)
            } else {
                selection$unformattedHandicap = as.numeric(selection$unformattedHandicap)
                handicap = c(handicap, selection$unformattedHandicap)
            }
        }
    }
    
    # Output
    tibble(prop_market_name,
           selection_name_prop,
           prop_market_selection,
           prop_market_price,
           player_id,
           market_id,
           handicap,
           url)
}

# Safe version that just returns NULL if there is an error
safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)

# Helper function to clean player names
clean_player_name <- function(player_name) {
  player_name <- case_when(
    player_name == "P.J Washington" ~ "P.J. Washington",
    player_name == "Bruce Brown Jr" ~ "Bruce Brown",
    player_name == "Wendell Carter" ~ "Wendell Carter Jr.",
    player_name == "Jabari Smith" ~ "Jabari Smith Jr.",
    player_name == "Bogdan Bogdanovic" ~ "Bogdan Bogdanović",
    player_name == "Cam Johnson" ~ "Cameron Johnson",
    player_name == "Dennis Schroder" ~ "Dennis Schröder",
    player_name == "Jakob Poeltl" ~ "Jakob Pöltl",
    player_name == "Jusuf Nurkic" ~ "Jusuf Nurkić",
    player_name == "Luka Doncic" ~ "Luka Dončić",
    player_name == "Nikola Jokic" ~ "Nikola Jokić",
    player_name == "Nikola Jovic" ~ "Nikola Jović",
    player_name == "Nikola Vucevic" ~ "Nikola Vučević",
    player_name == "Dereck Lively" ~ "Dereck Lively II",
    TRUE ~ player_name  # Default case to return the original player name
  )
  return(player_name)
}

#===============================================================================
# Player Goals
#===============================================================================

# Map function to player points urls
player_goals_data <-
    map(player_goals_links, safe_read_prop_url)

# Get just result part from output
player_goals_data <-
    player_goals_data |>
    map("result") |>
    map_df(bind_rows)

# Add market name
player_goals_data <-
    player_goals_data |>
    filter(str_detect(prop_market_name, "Qtr", negate = TRUE)) |>
    mutate(market_name = "Player goals") |> 
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    left_join(team_names, by = "match_id") |> 
    mutate(match = paste(home_team, "v", away_team)) |> 
    left_join(player_prop_metadata)

# Get player goals alternate lines---------------------------------------------

player_goals_alternate <-
    player_goals_data |>
    filter(str_detect(prop_market_name, "Any Time|2\\+")) |>
    mutate(line = ifelse(prop_market_name == " Any Time Goal Scorer ", 0.5, 1.5)) |>
    mutate(line = as.numeric(line)) |>
    rename(player_name = selection_name_prop) |>
    mutate(
        player_name = clean_player_name(player_name)) |>
    rename(over_price = prop_market_price) |>
    relocate(match, .before = player_name) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player goals",
        player_name,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id
    )

# Get player goals over / under -----------------------------------------------

player_goals_over <-
    player_goals_data |> 
    filter(str_detect(selection_name_prop, "Over")) |>
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Over ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " \\- Over")) |>
    mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
    rename(line = handicap) |> 
  mutate(
    player_name = clean_player_name(player_name)) |>
    relocate(match, .before = player_name) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player goals",
        player_name,
        line,
        over_price = prop_market_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id
    )

player_goals_under <-
  player_goals_data |> 
  filter(str_detect(selection_name_prop, "Under")) |>
  separate(selection_name_prop, into = c("name_2", "line_2"), sep = " under ", remove = FALSE) |>
  mutate(line_2 = as.numeric(line_2)) |>
  mutate(handicap = coalesce(handicap, line_2)) |>
  rename(player_name = selection_name_prop) |> 
  mutate(player_name = str_remove(player_name, " \\- Under")) |>
  mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
  rename(line = handicap) |> 
  mutate(
    player_name = clean_player_name(player_name)) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player goals",
    player_name,
    line,
    under_price = prop_market_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id
  )

# Combine
player_goals_over_under <-
    player_goals_over |>
    left_join(player_goals_under) |> 
  relocate(under_price, .after = over_price)

#===============================================================================
# Player Points
#===============================================================================

# Map function to player points URLs safely
player_points_data <-
  map(player_points_links, safe_read_prop_url)

# Extract result part from output and handle empty cases
player_points_data <-
  player_points_data |>
  map("result") |>
  map_df(~ if (is.null(.x) || nrow(.x) == 0) {
    tibble(
      prop_market_name = character(),
      url = character(),
      player = character(),
      handicap = numeric(),
      selection_name_prop = character(),
      market_id = numeric(),
      player_id = numeric(),
      prop_market_price = numeric()
    )
  } else {
    .x
  })

# Add market name
player_points_data <-
  player_points_data |>
  filter(str_detect(prop_market_name, "Qtr", negate = TRUE)) |>
  mutate(market_name = "Player points") |> 
  mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
  rename(match_id = url) |> 
  mutate(match_id = as.numeric(match_id)) |> 
  left_join(team_names, by = "match_id") |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_prop_metadata)

# Get player points alternate lines---------------------------------------------

player_points_alternate <-
  player_points_data |>
  filter(str_detect(prop_market_name, "\\+ Points")) |>
  mutate(line = str_extract(prop_market_name, "\\d+") |> as.numeric()) |>
  rename(player_name = selection_name_prop) |>
  mutate(
    player_name = clean_player_name(player_name)
  ) |>
  rename(over_price = prop_market_price) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player points",
    player_name,
    line = as.numeric(line) - 0.5,
    over_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id,
    player_id
  )

# Get player points over / under -----------------------------------------------

player_points_over <-
  player_points_data |> 
  filter(str_detect(selection_name_prop, "Over")) |>
  separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Over ", remove = FALSE) |>
  mutate(line_2 = as.numeric(line_2)) |>
  mutate(handicap = coalesce(handicap, line_2)) |>
  rename(player_name = selection_name_prop) |> 
  mutate(player_name = str_remove(player_name, " \\- Over")) |>
  mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
  rename(line = handicap) |> 
  mutate(
    player_name = clean_player_name(player_name)
  ) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player points",
    player_name,
    line,
    over_price = prop_market_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id
  )

player_points_under <-
  player_points_data |> 
  filter(str_detect(selection_name_prop, "Under")) |>
  separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Under ", remove = FALSE) |>
  mutate(line_2 = as.numeric(line_2)) |>
  mutate(handicap = coalesce(handicap, line_2)) |>
  rename(player_name = selection_name_prop) |> 
  mutate(player_name = str_remove(player_name, " \\- Under")) |>
  mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
  rename(line = handicap) |> 
  mutate(
    player_name = clean_player_name(player_name)
  ) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player points",
    player_name,
    line,
    under_price = prop_market_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id
  )

# Combine
player_points_over_under <-
  player_points_over |>
  left_join(player_points_under) |> 
  relocate(under_price, .after = over_price)

#===============================================================================
# Player Shots on Goal
#===============================================================================

# Map function to player shots on goal URLs
player_shots_on_goal_data <-
  map(player_shots_on_goal_links, safe_read_prop_url)

# Extract result part from output and handle empty cases
player_shots_on_goal_data <-
  player_shots_on_goal_data |>
  map("result") |>
  map_df(~ if (is.null(.x) || nrow(.x) == 0) {
    tibble(
      prop_market_name = character(),
      url = character(),
      player = character(),
      handicap = numeric(),
      selection_name_prop = character(),
      market_id = numeric(),
      player_id = numeric(),
      prop_market_price = numeric()
    )
  } else {
    .x
  })

# Add market name
player_shots_on_goal_data <-
  player_shots_on_goal_data |>
  filter(str_detect(prop_market_name, "Qtr", negate = TRUE)) |>
  mutate(market_name = "Player shots on goal") |> 
  mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
  rename(match_id = url) |> 
  mutate(match_id = as.numeric(match_id)) |> 
  left_join(team_names, by = "match_id") |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_prop_metadata)

# Get player shots on goal alternate lines--------------------------------------

player_shots_on_goal_alternate <-
  player_shots_on_goal_data |>
  filter(str_detect(prop_market_name, "\\+ Shots on Goal")) |>
  filter(str_detect(prop_market_name, "60 Min", negate = TRUE)) |>
  filter(str_detect(prop_market_name, "1st Period", negate = TRUE)) |>
  mutate(line = str_extract(prop_market_name, "\\d+") |> as.numeric()) |>
  rename(player_name = selection_name_prop) |>
  mutate(
    player_name = clean_player_name(player_name)
  ) |>
  rename(over_price = prop_market_price) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player shots on goal",
    player_name,
    line = as.numeric(line) - 0.5,
    over_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id,
    player_id
  )

# Get player shots on goal over / under ----------------------------------------

player_shots_on_goal_over <-
  player_shots_on_goal_data |> 
  filter(str_detect(selection_name_prop, "Over")) |>
  filter(str_detect(prop_market_name, "60 Min", negate = TRUE)) |>
  filter(str_detect(prop_market_name, "1st Period", negate = TRUE)) |>
  separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Over ", remove = FALSE) |>
  mutate(line_2 = as.numeric(line_2)) |>
  mutate(handicap = coalesce(handicap, line_2)) |>
  rename(player_name = selection_name_prop) |> 
  mutate(player_name = str_remove(player_name, " \\- Over")) |>
  mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
  rename(line = handicap) |> 
  mutate(
    player_name = clean_player_name(player_name)
  ) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player shots on goal",
    player_name,
    line,
    over_price = prop_market_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id
  )

player_shots_on_goal_under <-
  player_shots_on_goal_data |> 
  filter(str_detect(selection_name_prop, "Under")) |>
  filter(str_detect(prop_market_name, "60 Min", negate = TRUE)) |>
  filter(str_detect(prop_market_name, "1st Period", negate = TRUE)) |>
  separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Under ", remove = FALSE) |>
  mutate(line_2 = as.numeric(line_2)) |>
  mutate(handicap = coalesce(handicap, line_2)) |>
  rename(player_name = selection_name_prop) |> 
  mutate(player_name = str_remove(player_name, " \\- Under")) |>
  mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
  rename(line = handicap) |> 
  mutate(
    player_name = clean_player_name(player_name)
  ) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player shots on goal",
    player_name,
    line,
    under_price = prop_market_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id
  )

# Combine
player_shots_on_goal_over_under <-
  player_shots_on_goal_over |>
  left_join(player_shots_on_goal_under) |> 
  relocate(under_price, .after = over_price)

#===============================================================================
# Player Assists
#===============================================================================

# Map function to player assists URLs
player_assists_data <-
  map(player_assists_links, safe_read_prop_url)

# Extract result part from output and handle empty cases
player_assists_data <-
  player_assists_data |>
  map("result") |>
  map_df(~ if (is.null(.x) || nrow(.x) == 0) {
    tibble(
      prop_market_name = character(),
      url = character(),
      player = character(),
      handicap = numeric(),
      selection_name_prop = character(),
      market_id = numeric(),
      player_id = numeric(),
      prop_market_price = numeric()
    )
  } else {
    .x
  })

# Add market name
player_assists_data <-
  player_assists_data |>
  filter(str_detect(prop_market_name, "Qtr", negate = TRUE)) |>
  mutate(market_name = "Player assists") |> 
  mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
  rename(match_id = url) |> 
  mutate(match_id = as.numeric(match_id)) |> 
  left_join(team_names, by = "match_id") |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_prop_metadata)

# Get player assists alternate lines--------------------------------------------

player_assists_alternate <-
  player_assists_data |>
  filter(str_detect(prop_market_name, "\\+ Assists")) |>
  mutate(line = str_extract(prop_market_name, "\\d+") |> as.numeric()) |>
  rename(player_name = selection_name_prop) |>
  mutate(
    player_name = clean_player_name(player_name)
  ) |>
  rename(over_price = prop_market_price) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player assists",
    player_name,
    line = as.numeric(line) - 0.5,
    over_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id,
    player_id
  )

# Get player assists over / under ----------------------------------------------

player_assists_over <-
  player_assists_data |> 
  filter(str_detect(selection_name_prop, "Over")) |>
  separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Over ", remove = FALSE) |>
  mutate(line_2 = as.numeric(line_2)) |>
  mutate(handicap = coalesce(handicap, line_2)) |>
  rename(player_name = selection_name_prop) |> 
  mutate(player_name = str_remove(player_name, " \\- Over")) |>
  mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
  rename(line = handicap) |> 
  mutate(
    player_name = clean_player_name(player_name)
  ) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player assists",
    player_name,
    line,
    over_price = prop_market_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id
  )

player_assists_under <-
  player_assists_data |> 
  filter(str_detect(selection_name_prop, "Under")) |>
  separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Under ", remove = FALSE) |>
  mutate(line_2 = as.numeric(line_2)) |>
  mutate(handicap = coalesce(handicap, line_2)) |>
  rename(player_name = selection_name_prop) |> 
  mutate(player_name = str_remove(player_name, " \\- Under")) |>
  mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
  rename(line = handicap) |> 
  mutate(
    player_name = clean_player_name(player_name)
  ) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player assists",
    player_name,
    line,
    under_price = prop_market_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id
  )

# Combine
player_assists_over_under <-
  player_assists_over |>
  left_join(player_assists_under) |> 
  relocate(under_price, .after = over_price)
#===============================================================================
# Write to CSV
#===============================================================================

# Goals
player_goals_alternate |>
  bind_rows(player_goals_over_under) |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "line",
    "over_price",
    "under_price",
    "agency"
  ) |>
  mutate(market_name = "Player Goals") |>
  mutate(agency = "Sportsbet") |> 
  write_csv("Data/scraped_odds/sportsbet_player_goals.csv")

# Points
player_points_alternate |>
  bind_rows(player_points_over_under) |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "line",
    "over_price",
    "under_price",
    "agency"
  ) |>
  mutate(market_name = "Player Points") |>
  mutate(agency = "Sportsbet") |> 
  write_csv("Data/scraped_odds/sportsbet_player_points.csv")

# Shots on Goal
player_shots_on_goal_alternate |>
  bind_rows(player_shots_on_goal_over_under) |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "line",
    "over_price",
    "under_price",
    "agency"
  ) |>
  mutate(market_name = "Player Shots on Goal") |>
  mutate(agency = "Sportsbet") |> 
  write_csv("Data/scraped_odds/sportsbet_player_shots_on_goal.csv")

# Assists
player_assists_alternate |>
  bind_rows(player_assists_over_under) |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "line",
    "over_price",
    "under_price",
    "agency"
  ) |>
  mutate(market_name = "Player Assists") |>
  mutate(agency = "Sportsbet") |> 
  write_csv("Data/scraped_odds/sportsbet_player_assists.csv")

}

##%######################################################%##
#                                                          #
####                Run functions safely                ####
#                                                          #
##%######################################################%##

safe_main_markets <- safely(main_markets_function, otherwise = NULL)
safe_player_props <- safely(player_props_function, otherwise = NULL)

safe_main_markets()
safe_player_props()
