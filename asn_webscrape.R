library(rvest)
library(dplyr)

# Function to scrape all pages for a given year
scrape_asn_year <- function(year) {
  base_url <- paste0("https://asn.flightsafety.org/database/year/", year, "/")
  all_data <- data.frame()
  page <- 1
  
  repeat {
    url <- paste0(base_url, page)
    message("Scraping: ", url)
    
    # Try reading the page; exit on error
    tryCatch({
      page_html <- read_html(url)
      
      # Attempt to find the accident table
      table_node <- page_html %>% html_node("table")
      
      if (is.na(table_node) || is.null(table_node)) {
        message("No table found. Ending scrape.")
        break
      }
      
      # Extract the table
      page_data <- table_node %>% html_table(fill = TRUE)
      
      # If empty or too small, assume end of data
      if (nrow(page_data) < 1) {
        message("Empty table found. Ending scrape.")
        break
      }
      
      # Add to cumulative data
      all_data <- bind_rows(all_data, page_data)
      page <- page + 1
      
    }, error = function(e) {
      message("Error on page ", page, ": ", e$message)
      break
    })
  }
  
  return(all_data)
}

# Example: Scrape accidents from 1919
accidents_1919 <- scrape_asn_year(1919)

# View the first few rows
head(accidents_1919)
