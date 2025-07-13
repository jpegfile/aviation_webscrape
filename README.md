Bureau of Aircraft Accidents Archives (B3A): https://www.baaa-acro.com/crash-archives

You can scrape directly from my RShiny app: jpegfile.shinyapps.io/B3A_Webscrape/

Files:
-> baaa_webscrape.R 
  - Scrapes the table and the report page
  - Scrapres the operators in the table, both from image and text

-> app.R
  - Shiny app code for baaa_webscrape.R
