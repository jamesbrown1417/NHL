library(tidyverse)

# Create function to fix NHL Team Names
fix_nhl_team_names <- function(team_vector) {
  new_vector <- case_when(
    str_detect(team_vector, "(Anaheim)|(Ducks)") ~ "Anaheim Ducks",
    str_detect(team_vector, "(Arizona)|(Coyotes)") ~ "Arizona Coyotes",
    str_detect(team_vector, "(Boston)|(Bruins)") ~ "Boston Bruins",
    str_detect(team_vector, "(Buffalo)|(Sabres)") ~ "Buffalo Sabres",
    str_detect(team_vector, "(Calgary)|(Flames)") ~ "Calgary Flames",
    str_detect(team_vector, "(Carolina)|(Hurricanes)") ~ "Carolina Hurricanes",
    str_detect(team_vector, "(Chicago)|(Blackhawks)") ~ "Chicago Blackhawks",
    str_detect(team_vector, "(Colorado)|(Avalanche)") ~ "Colorado Avalanche",
    str_detect(team_vector, "(Columbus)|(Blue Jackets)") ~ "Columbus Blue Jackets",
    str_detect(team_vector, "(Dallas)|(Stars)") ~ "Dallas Stars",
    str_detect(team_vector, "(Detroit)|(Red Wings)") ~ "Detroit Red Wings",
    str_detect(team_vector, "(Edmonton)|(Oilers)") ~ "Edmonton Oilers",
    str_detect(team_vector, "(Florida)|(Panthers)") ~ "Florida Panthers",
    str_detect(team_vector, "(Los Angeles)|(Kings)") ~ "Los Angeles Kings",
    str_detect(team_vector, "(Minnesota)|(Wild)") ~ "Minnesota Wild",
    str_detect(team_vector, "(Montreal)|(Canadiens)") ~ "Montreal Canadiens",
    str_detect(team_vector, "(Nashville)|(Predators)") ~ "Nashville Predators",
    str_detect(team_vector, "(New Jersey)|(Devils)") ~ "New Jersey Devils",
    str_detect(team_vector, "(New York Islanders)|(Islanders)") ~ "New York Islanders",
    str_detect(team_vector, "(New York Rangers)|(Rangers)") ~ "New York Rangers",
    str_detect(team_vector, "(Ottawa)|(Senators)") ~ "Ottawa Senators",
    str_detect(team_vector, "(Philadelphia)|(Flyers)") ~ "Philadelphia Flyers",
    str_detect(team_vector, "(Pittsburgh)|(Penguins)") ~ "Pittsburgh Penguins",
    str_detect(team_vector, "(San Jose)|(Sharks)") ~ "San Jose Sharks",
    str_detect(team_vector, "(Seattle)|(Kraken)") ~ "Seattle Kraken",
    str_detect(team_vector, "(St\\. Louis)|(Blues)") ~ "St. Louis Blues",
    str_detect(team_vector, "(Tampa Bay)|(Lightning)") ~ "Tampa Bay Lightning",
    str_detect(team_vector, "(Toronto)|(Maple Leafs)") ~ "Toronto Maple Leafs",
    str_detect(team_vector, "(Vancouver)|(Canucks)") ~ "Vancouver Canucks",
    str_detect(team_vector, "(Vegas)|(Golden Knights)") ~ "Vegas Golden Knights",
    str_detect(team_vector, "(Washington)|(Capitals)") ~ "Washington Capitals",
    str_detect(team_vector, "(Winnipeg)|(Jets)") ~ "Winnipeg Jets",
    TRUE ~ team_vector  # Return the original value if no match is found
  )
  return(new_vector)
}