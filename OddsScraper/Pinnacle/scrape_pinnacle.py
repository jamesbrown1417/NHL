import http.client
import json
import zlib
import pandas as pd

# API connection details
conn = http.client.HTTPSConnection("pinnacle-odds.p.rapidapi.com")

headers = {
    'x-rapidapi-key': "2cd51286d3mshb39e33e7d200279p186b64jsn2df1d1eaa2fa",  # Store this key securely
    'x-rapidapi-host': "pinnacle-odds.p.rapidapi.com",
    'Accept-Encoding': 'gzip'  # Request gzip encoding
}

# Send the GET request for NHL (sport_id=4)
conn.request("GET", "/kit/v1/special-markets?is_have_odds=true&sport_id=4", headers=headers)

# Get the response
res = conn.getresponse()
data = res.read()

# Decompress if response is gzip encoded
if res.getheader('Content-Encoding') == 'gzip':
    data = zlib.decompress(data, zlib.MAX_WBITS | 16)  # Decompress gzip

# Decode and parse JSON response
try:
    json_data = json.loads(data.decode("utf-8"))
except (json.JSONDecodeError, zlib.error) as e:
    print(f"Error processing data: {e}")
    exit()

# Function to extract player prop data
def extract_market_data(json_data, market_filter, file_name):
    market_dfs = []

    for event in json_data['specials']:
        if event['category'] == "Player Props" and market_filter in event.get('name', ''):
            name = event['name']
            home_team = event['event']['home']
            away_team = event['event']['away']
            market_df = pd.DataFrame(event['lines']).T
            market_df['selection'] = name
            market_df['home_team'] = home_team
            market_df['away_team'] = away_team
            market_dfs.append(market_df)

    if market_dfs:
        final_df = pd.concat(market_dfs, ignore_index=True)
        final_df.to_csv(f"OddsScraper/Pinnacle/{file_name}.csv", index=False)
        print(f"Saved {file_name}.csv successfully!")
    else:
        print(f"No data found for {market_filter}")

# Extract data for NHL markets

# Player Shots on Goal
extract_market_data(json_data, "Shots", "pinnacle_shots_on_goal_raw")

# Player Assists
extract_market_data(json_data, "Assists", "pinnacle_assists_raw")

# Player Points
extract_market_data(json_data, "Points", "pinnacle_points_raw")

# Player Goals
extract_market_data(json_data, "Goals", "pinnacle_goals_raw")

print("NHL player prop data extraction completed.")