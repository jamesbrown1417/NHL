#!/bin/bash

# Give access to normal path vars
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd ~/Projects/NHL || exit

# Remove .json and .txt files in specific directories
rm OddsScraper/Neds/*.json
rm Data/BET365_HTML/*.txt

# Execute Python and R scripts - Neds
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Neds/get_neds_urls.py
Rscript OddsScraper/Neds/get_neds_match_urls.R
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Neds/get_match_json.py
Rscript OddsScraper/Neds/scrape_neds.R

# Execute Python and R scripts - Bet365
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/get_bet365_html.py
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/get_bet365_player.py
Rscript OddsScraper/scrape_bet365.R

# Execute Python and R scripts - Pinnacle
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Pinnacle/scrape_pinnacle.py
Rscript OddsScraper/Pinnacle/tidy_pinnacle.R

# Run Sportsbet R Script
Rscript OddsScraper/scrape_sportsbet.R

# Run Top Down Script
Rscript Scripts/top_down_bets.R

# Publish report using Quarto
echo "1" | quarto publish quarto-pub Reports/nhl_arbs.qmd

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage"

# Push the commit to the 'main' branch on 'origin'
git push origin main