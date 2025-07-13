Bureau of Aircraft Accidents Archives (B3A): https://www.baaa-acro.com/crash-archives

Files:
-> baaa_webscrape.R 
  - Scrapes the table and the report page
  - Scrapres the operators in the table, both from image and text

-> app.R
  - Shiny app code for baaa_webscrape.R

The "date" columns data type is character as CSV does not hold the data types in it. To convert "date" column to date data type:

install.packages("lubridate")
  library(lubridate)
  <dataset_name>$date <- dmy(<dataset_namme>$date)
