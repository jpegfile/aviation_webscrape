library(shiny)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(DT)
library(rsconnect)

## ---- Functions ----

# Scrape the detailed report page
scrape_report <- function(url) {
  if (is.na(url) || is.null(url)) return(data.frame())
  
  page <- tryCatch(read_html(url), error = function(e) return(NULL))
  if (is.null(page)) return(data.frame())
  
  extract_text <- function(css) {
    node <- page %>% html_node(paste0("div.", css))
    if (length(node) == 0 || is.na(node) || is.null(node)) return(NA)
    text <- node %>% html_text2() %>% trimws()
    text <- sub("^[^:]*:\\s*", "", text)  # Remove the label before colon
    if (text == "") return(NA) else return(text)
  }
  
  data.frame(
    date_time        = extract_text("crash-date"),
    aircraft_type_r  = extract_text("crash-aircraft"),
    flight_phase     = extract_text("crash-flight-phase"),
    flight_type      = extract_text("crash-flight-type"),
    survivors        = extract_text("crash-survivors"),
    site             = extract_text("crash-site"),
    YOM              = extract_text("crash-yom"),
    country          = extract_text("crash-country"),
    region           = extract_text("crash-region"),
    crew_fatalities  = extract_text("crash-crew-fatalities"),
    pax_fatalities   = extract_text("crash-pax-fatalities"),
    total_fatalities = extract_text("crash-total-fatalities"),
    stringsAsFactors = FALSE
  )
}

# Scrape a single page from the archive
scrape_page <- function(year, page) {
  url <- paste0(
    "https://www.baaa-acro.com/crash-archives?",
    "created=", year, "-01-01&created_1=", year, "-12-31&page=", page
  )
  
  message("Scraping page: ", url)
  
  page_html <- tryCatch(read_html(url), error = function(e) return(NULL))
  if (is.null(page_html)) return(NULL)
  
  table_node <- page_html %>% html_node(".table-responsive")
  if (is.null(table_node)) return(NULL)
  
  rows <- table_node %>% html_nodes("tbody tr")
  if (length(rows) == 0) return(NULL)
  
  data <- lapply(rows, function(row) {
    cells <- row %>% html_nodes("td")
    cells <- cells[-1]  # Skip crash image column
    
    if (length(cells) < 7) return(NULL)
    
    date <- cells[1] %>% html_text2() %>% trimws()
    
    # Operator
    operator_cell <- cells[2]
    operator_text <- operator_cell %>% html_text2() %>% trimws()
    if (operator_text == "") {
      img_node <- operator_cell %>% html_node("img")
      if (!is.na(img_node) && !is.null(img_node)) {
        img_src <- img_node %>% html_attr("src")
        filename <- basename(img_src)
        filename <- gsub(".jpg", "", filename, ignore.case = TRUE)
        filename <- gsub("%20", " ", filename, fixed = TRUE)
        operator <- filename
      } else {
        operator <- NA
      }
    } else {
      operator <- operator_text
    }
    
    aircraft_type <- cells[3] %>% html_text2() %>% trimws()
    location <- cells[4] %>% html_text2() %>% trimws()
    fatalities <- cells[5] %>% html_text2() %>% trimws()
    registration <- cells[6] %>% html_text2() %>% trimws()
    
    report_url <- cells[length(cells)] %>%
      html_node("a") %>%
      html_attr("href")
    if (!is.na(report_url) && !is.null(report_url)) {
      report_url <- paste0("https://www.baaa-acro.com", report_url)
    } else {
      report_url <- NA
    }
    
    data.frame(
      date = date,
      operator = operator,
      aircraft_type = aircraft_type,
      location = location,
      fatalities = fatalities,
      registration = registration,
      report_url = report_url,
      stringsAsFactors = FALSE
    )
  })
  
  df <- do.call(rbind, data)
  return(df)
}

# Scrape an entire year with reports
scrape_year_with_reports <- function(year) {
  message("Scraping year: ", year)
  page <- 0
  all_data <- list()
  
  repeat {
    page_data <- scrape_page(year, page)
    if (is.null(page_data) || nrow(page_data) == 0) break
    
    report_data <- lapply(page_data$report_url, scrape_report) %>% bind_rows()
    
    combined <- cbind(page_data, report_data)
    all_data[[length(all_data) + 1]] <- combined
    
    page <- page + 1
  }
  
  if (length(all_data) == 0) {
    return(NULL)
  } else {
    return(bind_rows(all_data))
  }
}

## ---- UI ----

ui <- fluidPage(
  titlePanel("B3A Aircraft Accident Scraper"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("start_year", "Start Year", value = 2020, min = 1918),
      numericInput("end_year", "End Year (optional)", value = NA, min = 1918),
      actionButton("scrape", "Start Scraping"),
      downloadButton("download", "Download .csv"),
      br(),
      br(),
      textOutput("status")
    ),
    
    mainPanel(
      DT::dataTableOutput("preview")
    )
  )
)

## ---- Server ----

server <- function(input, output, session) {
  scraped_data <- reactiveVal(NULL)
  
  observeEvent(input$scrape, {
    start_year <- input$start_year
    end_year <- input$end_year
    
    if (is.na(start_year)) {
      output$status <- renderText("❌ Please enter a start year.")
      return()
    }
    
    if (is.na(end_year) || end_year < start_year) {
      years <- start_year
    } else {
      years <- start_year:end_year
    }
    
    output$status <- renderText("⏳ Scraping in progress...")
    
    data_list <- list()
    
    withProgress(message = 'Scraping...', value = 0, {
      for (i in seq_along(years)) {
        year <- years[i]
        incProgress(1/length(years), detail = paste("Year", year))
        year_data <- scrape_year_with_reports(year)
        if (!is.null(year_data)) {
          data_list[[length(data_list) + 1]] <- year_data
        }
      }
    })
    
    if (length(data_list) == 0) {
      output$status <- renderText("⚠️ No data scraped.")
      scraped_data(NULL)
    } else {
      final_data <- bind_rows(data_list) %>%
        select(-date_time) %>%
        mutate(date = lubridate::mdy(date) |> format("%d.%m.%Y"))
      
      scraped_data(final_data)
      output$status <- renderText(
        paste0("✅ Scraping complete. Total rows: ", nrow(final_data))
      )
    }
  })
  
  output$preview <- DT::renderDataTable({
    scraped_data()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("BAAA_scraped_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(scraped_data(), file, row.names = FALSE)
    }
  )
}

## ---- Run App ----

shinyApp(ui, server)
