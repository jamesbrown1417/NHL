# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime
import pandas as pd
import asyncio

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Read in CSV of URLs=========================================================
# Read csv (no header col)
url_df = pd.read_csv('Data/BET365_HTML/urls.csv', header=None)

# Convert first column to a list and keep only first 16 URLs for now
url_df = url_df[0][:16]

# Get H2H HTML===============================================================

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        for index, url in enumerate(url_df, start=1):  # Start counting from 1 for match_n
            try:
                await driver.get(url)
                await driver.sleep(2)

                # Click relevant hockey prop buttons
                hockey_buttons = [
                    "Shots on Goal",
                  # "Shots on Goal O/U",
                    "Points",
                    "Points O/U",
                    "Assists",
                    "Assists O/U"
                ]

                for button_text in hockey_buttons:
                    try:
                        button = await driver.find_element(
                            By.XPATH, f"//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='{button_text}']"
                        )
                        await driver.execute_script("arguments[0].scrollIntoView(true);", button)
                        await driver.execute_script("window.scrollBy(0, -150)")
                        await button.click()
                        await driver.sleep(1)
                        print(f"Clicked button: {button_text}")
                    except:
                        print(f"Button not found: {button_text}")
                        pass

                # Click 'Show more' buttons if present
                button_elements = await driver.find_elements(
                    By.XPATH, "//div[contains(@class, 'msl-ShowMore_Link ') and contains(text(), 'Show more')]"
                )

                print(f"Found {len(button_elements)} 'Show more' buttons.")
                    
                for button_element in button_elements:
                    await driver.execute_script("arguments[0].scrollIntoView(true);", button_element)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await button_element.click()
                    await driver.sleep(2)
                    print("Clicked 'Show more' button.")

                # Write out HTML to file------------------------------------------------
                elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
                body_html_players = await elem.get_attribute('outerHTML')
                with open(f"Data/BET365_HTML/body_html_hockey_match_{index}.txt", 'w') as f:
                    f.write(body_html_players)
                        
            except Exception as e:
                print(f"An error occurred with URL {url}: {e}. Moving to the next URL.")
                continue  # Proceed to the next iteration of the loop

asyncio.run(main())