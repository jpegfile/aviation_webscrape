library(rvest)
library(dplyr)

# Function to scrape all pages for a given year
scrape_asn_year <- function(year) {
  base_url <- paste0("https://asn.flightsafety.org/database/year/", year, "/")
  all_data <- data.frame()
  page <- 1
  keep_scraping <- TRUE
  
  while (keep_scraping) {
    url <- paste0(base_url, page)
    message("Scraping: ", url)
    
    result <- tryCatch({
      page_html <- read_html(url)
      
      table_node <- page_html %>% html_node("table")
      if (is.na(table_node) || is.null(table_node)) {
        message("No table found.")
        return(NULL)
      }
      
      page_data <- table_node %>% html_table(fill = TRUE)
      
      if (nrow(page_data) < 1) {
        message("Empty table.")
        return(NULL)
      }
      
      # Convert all columns to character
      page_data <- page_data %>% mutate(across(everything(), as.character))
      page_data$Year <- year
      
      return(page_data)
    }, error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    })
    
    # If result is NULL, stop the loop
    if (is.null(result)) {
      keep_scraping <- FALSE
    } else {
      all_data <- bind_rows(all_data, result)
      page <- page + 1
      Sys.sleep(runif(1, min = 1.5, max = 3.5))
    }
  }
  
  return(all_data)
}

# Example: Scrape accidents from 1919
accidents_1919 <- scrape_asn_year(1919)
