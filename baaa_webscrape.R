library(rvest)
library(dplyr)
library(stringr)

# Build the filtered URL for a given year and page number
build_url <- function(year, page = 0) {
  base <- "https://www.baaa-acro.com/crash-archives"
  query <- paste0(
    "?created=", year, "-01-01",
    "&created_1=", year, "-12-31",
    "&field_crash_region_target_id=All",
    "&field_crash_country_target_id=",
    "&field_crash_registration_target_id=",
    "&field_crash_aircraft_target_id=",
    "&field_crash_operator_target_id=",
    "&field_crash_cause_target_id=All",
    "&field_crash_zone_target_id=",
    "&field_crash_site_type_target_id=All",
    "&field_crash_phase_type_target_id=All",
    "&field_crash_flight_type_target_id=All",
    "&field_crash_survivors_value=All",
    "&field_crash_city_target_id=",
    "&page=", page
  )
  paste0(base, query)
}

# Scrape summary table on one page and get desired columns + report URLs
scrape_page <- function(year, page) {
  url <- build_url(year, page)
  message("Scraping summary page: ", url)
  
  page_html <- read_html(url)
  
  div_node <- page_html %>% html_node("div.table-responsive")
  if (is.null(div_node) || inherits(div_node, "xml_missing")) {
    message("No 'table-responsive' div found on page ", page)
    return(NULL)
  }
  
  table_node <- div_node %>% html_node("table")
  if (is.null(table_node) || inherits(table_node, "xml_missing")) {
    message("No table found inside 'table-responsive' div on page ", page)
    return(NULL)
  }
  
  df <- table_node %>% html_table(fill = TRUE)
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
  if (nrow(df) == 0) {
    message("No data rows found on page ", page)
    return(NULL)
  }
  
  # Extract report links (assumed in first <td> with <a>)
  links <- div_node %>%
    html_nodes("table tbody tr td:nth-child(1) a") %>%
    html_attr("href")
  
  base_url <- "https://www.baaa-acro.com"
  full_links <- paste0(base_url, links)
  
  # Identify columns by partial match
  cols <- names(df)
  date_col <- cols[grepl("date", tolower(cols))]
  ac_type_col <- cols[grepl("a/?c type|type", tolower(cols))]
  location_col <- cols[grepl("location", tolower(cols))]
  fatalities_col <- cols[grepl("fatalities", tolower(cols))]
  
  df_selected <- df %>%
    select(
      date = all_of(date_col),
      aircraft_type = all_of(ac_type_col),
      location = all_of(location_col),
      fatalities = all_of(fatalities_col)
    ) %>%
    mutate(
      fatalities = as.numeric(str_extract(fatalities, "\\d+")),
      report_url = full_links
    )
  
  return(df_selected)
}

# Scrape detailed report from one report URL
scrape_report <- function(url) {
  message("Scraping report details: ", url)
  page <- read_html(url)
  
  get_text_by_class <- function(class_name) {
    node <- page %>% html_node(paste0("div.", class_name))
    if (is.null(node) || inherits(node, "xml_missing")) return(NA_character_)
    text <- node %>% html_text(trim = TRUE)
    if (nchar(text) == 0) return(NA_character_)
    return(text)
  }
  
  # Clean each field by removing label and extra spaces
  clean_field <- function(text, label) {
    if (is.na(text)) return(NA_character_)
    cleaned <- sub(paste0("(?i)^", label, ":?\\s*"), "", text, perl = TRUE)
    cleaned <- trimws(cleaned)
    if (nchar(cleaned) == 0) return(NA_character_)
    return(cleaned)
  }
  
  date_time <- clean_field(get_text_by_class("crash-date"), "Date & time")
  aircraft_type <- clean_field(get_text_by_class("crash-aircraft"), "Type of aircraft")
  flight_phase <- clean_field(get_text_by_class("crash-flight-phase"), "Flight phase")
  flight_type <- clean_field(get_text_by_class("crash-flight-type"), "Flight type")
  survivors <- clean_field(get_text_by_class("crash-survivors"), "Survivors")
  site <- clean_field(get_text_by_class("crash-site"), "Site")
  YOM <- clean_field(get_text_by_class("crash-yom"), "YOM")
  country <- clean_field(get_text_by_class("crash-country"), "Country")
  region <- clean_field(get_text_by_class("crash-region"), "Region")
  crew_fatalities <- clean_field(get_text_by_class("crash-crew-fatalities"), "Crew fatalities")
  pax_fatalities <- clean_field(get_text_by_class("crash-pax-fatalities"), "Pax fatalities")
  total_fatalities <- clean_field(get_text_by_class("crash-total-fatalities"), "Total fatalities")
  
  tibble(
    report_url = url,
    date_time = date_time,
    aircraft_type = aircraft_type,
    flight_phase = flight_phase,
    flight_type = flight_type,
    survivors = survivors,
    site = site,
    YOM = YOM,
    country = country,
    region = region,
    crew_fatalities = crew_fatalities,
    pax_fatalities = pax_fatalities,
    total_fatalities = total_fatalities
  )
}



# Scrape all pages for a year, including detailed reports
scrape_year_with_reports <- function(year, max_pages = 100, delay = 1) {
  all_data <- list()
  page <- 0
  
  repeat {
    df_page <- scrape_page(year, page)
    
    if (is.null(df_page) || nrow(df_page) == 0) {
      message("No more data at page ", page, ". Stopping.")
      break
    }
    
    # Scrape detailed reports for this page
    details_list <- lapply(df_page$report_url, function(url) {
      Sys.sleep(delay)  # be polite with server!
      scrape_report(url)
    })
    details_df <- bind_rows(details_list)
    
    # Join details to summary data
    df_full <- left_join(df_page, details_df, by = "report_url")
    
    all_data[[length(all_data) + 1]] <- df_full
    
    page <- page + 1
    if (page >= max_pages) {
      message("Reached max pages limit (", max_pages, "), stopping.")
      break
    }
  }
  
  if (length(all_data) == 0) {
    message("No data found for year ", year)
    return(NULL)
  }
  
  bind_rows(all_data)
}

# Example usage:
result <- scrape_year_with_reports(1918)
View(result)
