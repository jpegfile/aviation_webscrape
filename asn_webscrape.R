library(rvest)
library(dplyr)

scrape_asn_year <- function(year) {
  base_url <- paste0("https://asn.flightsafety.org/database/year/", year, "/")
  all_data <- data.frame()
  page <- 1
  
  repeat {
    url <- paste0(base_url, page)
    message("Scraping: ", url)
    
    #read page; exit on error
    tryCatch({
      page_html <- read_html(url)
      
      #accident table
      table_node <- page_html %>% html_node("table")
      
      if (is.na(table_node) || is.null(table_node)) {
        message("No table found. Ending scrape.")
        break
      }
      
      #get table
      page_data <- table_node %>% html_table(fill = TRUE)
      
      #no data in the table, finish
      if (nrow(page_data) < 1) {
        message("Empty table found. Ending scrape.")
        break
      }
      
      #merge all data
      all_data <- bind_rows(all_data, page_data)
      page <- page + 1
      
    }, error = function(e) {
      message("Error on page ", page, ": ", e$message)
      break
    })
  }
  
  return(all_data)
}

accidents_1919 <- scrape_asn_year(1919)
head(accidents_1919)
