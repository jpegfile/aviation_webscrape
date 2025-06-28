library(rvest)
library(tidyverse)
library(stringr)
library(lubridate)

#b3a build url
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

#extract summary table and report links from one page
scrape_page <- function(year, page) {
  url <- build_url(year, page)
  message("Scraping summary page: ", url)
  
  page_html <- read_html(url)
  
  div_node <- page_html %>% html_node("div.table-responsive")
  if (is.null(div_node)) {
    message("No table found on page ", page)
    return(NULL)
  }
  
  table_node <- div_node %>% html_node("table")
  if (is.null(table_node)) {
    message("No table node found on page ", page)
    return(NULL)
  }
  
  df <- tryCatch({
    table_node %>% html_table(fill = TRUE)
  }, error = function(e) {
    message("Failed to parse table on page ", page)
    return(NULL)
  })
  
  if (is.null(df) || nrow(df) == 0) {
    message("Empty table on page ", page)
    return(NULL)
  }
  
  links <- div_node %>%
    html_nodes("table tbody tr td:nth-child(1) a") %>%
    html_attr("href")
  
  if (length(links) == 0) {
    message("No report links on page ", page)
    return(NULL)
  }
  
  base_url <- "https://www.baaa-acro.com"
  full_links <- paste0(base_url, links)
  
  df <- df %>%
    select(
      date = matches("date", ignore.case = TRUE),
      aircraft_type = matches("type", ignore.case = TRUE),
      location = matches("location", ignore.case = TRUE),
      fatalities = matches("fatalities", ignore.case = TRUE)
    ) %>%
    mutate(
      fatalities = as.numeric(str_extract(fatalities, "\\d+")),
      report_url = full_links
    )
  
  return(df)
}

#extract and clean detailed info from each report
scrape_report <- function(url) {
  message("Scraping report details: ", url)
  page <- read_html(url)
  
  get_text_by_class <- function(class_name) {
    node <- page %>% html_node(paste0("div.", class_name))
    if (is.null(node)) return(NA_character_)
    
    text <- node %>% html_text(trim = TRUE)
    if (is.na(text) || nchar(text) == 0) return(NA_character_)
    
    return(text)
  }
  
  clean_field <- function(text, label) {
    if (is.na(text)) return(NA_character_)
    cleaned <- sub(paste0("(?i)^", label, ":?\\s*"), "", text, perl = TRUE)
    cleaned <- trimws(cleaned)
    if (is.na(cleaned) || nchar(cleaned) == 0) return(NA_character_)
    return(cleaned)
  }
  
  parse_date_time <- function(text) {
    date_match <- str_match(text, "([A-Za-z]+\\s+\\d{1,2},\\s*\\d{4})")
    time_match <- str_match(text, "\\b(\\d{3,4})\\b")
    formatted_date <- formatted_time <- NA_character_
    
    if (!is.na(date_match[2])) {
      dt <- mdy(date_match[2])
      if (!is.na(dt)) formatted_date <- format(dt, "%d.%m.%Y")
    }
    if (!is.na(time_match[2])) {
      time <- str_pad(time_match[2], 4, pad = "0")
      formatted_time <- paste0(substr(time, 1, 2), ":", substr(time, 3, 4))
    }
    list(date = formatted_date, time = formatted_time)
  }
  
  #clean scraped fields
  raw_date_time <- clean_field(get_text_by_class("crash-date"), "Date & time")
  parsed <- parse_date_time(raw_date_time)
  
  aircraft_type <- clean_field(get_text_by_class("crash-aircraft"), "Type of aircraft")
  flight_phase <- clean_field(get_text_by_class("crash-flight-phase"), "Flight phase")
  flight_type <- clean_field(get_text_by_class("crash-flight-type"), "Flight type")
  survivors <- clean_field(get_text_by_class("crash-survivors"), "Survivors")
  site <- clean_field(get_text_by_class("crash-site"), "Site")
  YOM_raw <- clean_field(get_text_by_class("crash-yom"), "YOM")
  country <- clean_field(get_text_by_class("crash-country"), "Country")
  region <- clean_field(get_text_by_class("crash-region"), "Region")
  crew_fatalities_raw <- clean_field(get_text_by_class("crash-crew-fatalities"), "Crew fatalities")
  pax_fatalities_raw <- clean_field(get_text_by_class("crash-pax-fatalities"), "Pax fatalities")
  total_fatalities_raw <- clean_field(get_text_by_class("crash-total-fatalities"), "Total fatalities")
  
  #numeric conversion
  extract_numeric <- function(text) {
    num <- str_extract(text, "\\d+")
    as.numeric(num)
  }
  
  YOM <- extract_numeric(YOM_raw)
  crew_fatalities <- extract_numeric(crew_fatalities_raw)
  pax_fatalities <- extract_numeric(pax_fatalities_raw)
  total_fatalities <- extract_numeric(total_fatalities_raw)
  
  tibble(
    report_url = url,
    date = parsed$date,
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

#full-year scraper combining summary and detailed info
scrape_year_with_reports <- function(year, max_pages = 5000, delay = 1) {
  all_data <- list()
  page <- 0
  
  repeat {
    df_summary <- scrape_page(year, page)
    if (is.null(df_summary) || nrow(df_summary) == 0) break
    
    details_list <- lapply(df_summary$report_url, function(url) {
      Sys.sleep(delay)
      scrape_report(url)
    })
    df_details <- bind_rows(details_list)
    
    df_combined <- left_join(df_summary, df_details, by = "report_url")
    
    all_data[[length(all_data) + 1]] <- df_combined
    page <- page + 1
    if (page >= max_pages) break
  }
  
  bind_rows(all_data)
}

#sometimes the site does not respond, this will try to scrape the year again, 5 times
retry_scrape <- function(year, max_attempts = 5, delay = 2) {
  attempt <- 1
  while (attempt <= max_attempts) {
    message("Attempt ", attempt, " for year ", year)
    result <- tryCatch({
      scrape_year_with_reports(year, delay = delay)
    }, error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(result)) {
      message("✅ Success on attempt ", attempt, " for year ", year)
      return(result)
    }
    
    Sys.sleep(delay * attempt)  #increasing delay
    attempt <- attempt + 1
  }
  
  message("❌ Failed after ", max_attempts, " attempts for year ", year)
  return(NULL)
}

#start scrape here
years <- 2000:2024 #change year here
all_data <- list()

for (yr in years) {
  result <- retry_scrape(yr)
  
  if (!is.null(result) && nrow(result) > 0) {
    all_data[[as.character(yr)]] <- result
  } else {
    message("⚠️ No data for year ", yr)
  }
}

#2003 does not scrape in the loop for some reason
#so scrape it alone and merge with the rest of the data
df_2003 <- scrape_year_with_reports(2003, delay = 1)

final_dataset <- bind_rows(c(all_data, list(df_2003)))

#post cleaning
final_dataset <- bind_rows(all_data) %>%
  select(-date.x, -aircraft_type.x) %>%
  rename(date = date.y) %>%
  mutate(date = lubridate::dmy(date))

#IF NEEDED, this cleans the df_2003 the same way as final_dataset
df_2003 <- df_2003 %>%
  select(-date.x, -aircraft_type.x) %>%
  rename(date = date.y) %>%
  mutate(date = lubridate::dmy(date))

write.csv(final_dataset, "BAAA_2000_2024.csv", row.names = FALSE, fileEncoding = "UTF-8")

#diagnostics for scraped data: did all data scraped?
row_count_by_year <- final_dataset %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise(accidents = n()) %>%
  arrange(year)

print(row_count_by_year)
