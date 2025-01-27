from playwright.async_api import async_playwright
import asyncio
import os

# The URL pattern we are interested in
url_pattern = "https://api.neds.com.au/v2/sport/event-request?category_ids=%5B%22b7c1f944-d02b-4d9b-b6f3-cb31389cfe36%22%5D&include_any_team_vs_any_team_events=true"

async def close_browser(browser):
    """Close the browser."""
    await browser.close()
    print("Browser closed.")

async def main():
    async with async_playwright() as p:
        # Launch the browser
        browser = await p.chromium.launch()
        page = await browser.new_page()

        # Listen for the specific network response
        async def handle_response(response):
            if response.url == url_pattern:
                # Fetch and decode the response body
                body = await response.body()

                # Ensure the output directory exists
                output_dir = os.path.join("OddsScraper", "Neds")
                os.makedirs(output_dir, exist_ok=True)

                # Write out the body of the response to a file in UTF-8 encoding explicitly
                file_path = os.path.join(output_dir, "neds_response.json")
                with open(file_path, "w", encoding="utf-8") as f:
                    f.write(body.decode("utf-8"))

                # Print a message to the console
                print(f"Response captured and saved to {file_path}!")
                
                # Schedule browser to close
                asyncio.create_task(close_browser(browser))

        # Attach the handler to the 'response' event
        page.on('response', handle_response)

        # Navigate to the target page
        await page.goto("https://www.neds.com.au/sports/ice-hockey/usa/nhl", wait_until="networkidle")

        # Keep the script running until the browser is closed
        while len(await browser.contexts()) > 0:
            await asyncio.sleep(0.1)

# Run the script
asyncio.run(main())
