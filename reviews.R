# Load necessary libraries
library(rvest)  # Web scraping
library(tidyverse)  # Data manipulation
library(dplyr)  # Data manipulation
library(lubridate)  # Date manipulation
library(stringr)  # String manipulation
library(httr)  # HTTP requests
library(progress)  # Progress bar

# ==========================================================================================

# Load the dataframe containing the URLs of the agencies
agencies_data <-
  read.csv(
    "./data/dataframe_agencies.csv"
  )
# Extract the first 5 links from agencies_data$website
agency_urls <- head(agencies_data$page_url, 5)

# ==========================================================================================

# Function to scrape reviews from a single page
scrape_reviews_singlepage <- function(agency_url, from_page, to_page) {
  
  # Initialize lists
  review_titles <- list()
  review_dates_original <- list()
  review_ratings <- list()
  review_texts <- list()
  review_verified <- list()
  page_number <- list()
  agency_names <- list()
  
  # Initialize progress bar
  pb <- progress_bar$new(format = "[:bar] :percent :eta", total = to_page - from_page + 1)
  
  # Extract agency name outside the loop
  url <- paste0(agency_url, "?page=", from_page)
  webpage <- read_html(url)
  agency_name <- html_node(webpage, ".typography_display-s__qOjh6.typography_appearance-default__AAY17.title_displayName__TtDDM") %>% html_text()
  
  # Loop through each page
  for (i in from_page:to_page) {
    # Update progress bar
    pb$tick()
    # Construct the URL
    url <- paste0(agency_url, "?page=", i)
    # Pause for a random amount of time
    Sys.sleep(runif(1, 1, 3))
    
    tryCatch({
      webpage <- read_html(url)
      
      # Extract review elements
      reviews <- html_nodes(
        webpage,
        ".styles_cardWrapper__LcCPA.styles_show__HUXRb.styles_reviewCard__9HxJJ"
      )
      
      # Extract information from each review
      for (review in reviews) {
        # Review titles
        review_title <- html_node(review, '.typography_heading-s__f7029') %>% html_text()
        review_titles <- c(review_titles, review_title)
        
        # Review dates
        review_date_original <- html_node(review, 'time') %>% html_attr("datetime")
        review_dates_original <- c(review_dates_original, review_date_original)
        
        # Review ratings
        review_rating <- html_node(review, ".styles_reviewHeader__iU9Px") %>% html_attr("data-service-review-rating") %>% as.integer()
        review_ratings <- c(review_ratings, review_rating)
        
        # Review texts
        review_text <- html_node(review, '.typography_body-l__KUYFJ') %>% html_text()
        review_texts <- c(review_texts, review_text)
        
        # Review verified status
        review_verified_status <- ifelse(html_nodes(review, '.styles_detailsIcon__yqwWi') %>% length() > 0, TRUE, FALSE)
        review_verified <- c(review_verified, review_verified_status)
        
        # Page number
        page_number <- c(page_number, i)
      }
    }, error = function(e) {
      cat("Error occurred while processing page", i, ": ", conditionMessage(e), "\n")
    })
    
    # Check if it's time to pause
    if (i %% 200 == 0 && i != to_page) {
      cat("Pausing for 1 minute...\n")
      Sys.sleep(60)  # Pause for 1 minute1 (60 seconds)
    }
  }
  
  # Add agency name to the list
  agency_names <- rep(agency_name, length(review_titles))
  
  # Create final dataframe
  df_reviews <- tibble(
    review_title = review_titles,
    review_date_original = review_dates_original,
    review_rating = review_ratings,
    review_text = review_texts,
    review_verified = review_verified,
    page_number = page_number,
    agency_name = agency_names
  )
  
  return(df_reviews)
}

# Function to scrape all reviews from a list of agency URLs
scrape_all_reviews <- function(agency_urls) {
  full_df_reviews <- map_df(agency_urls, function(url) {
    # Read the HTML content of the page containing pagination
    pagination_page <- read_html(url)
    
    # Extract the last page number
    last_page <- pagination_page %>%
      html_node(xpath = "//a[span[contains(text(), 'Pagina successiva')]]/preceding-sibling::a[1]") %>%
      html_text() %>%
      as.integer()
    
    # Set Trustpilot page numbers to scrape here
    from_page <- 1
    to_page <- last_page
    
    # Scrape reviews
    partial_df <- scrape_reviews_singlepage(url, from_page, to_page)
    
    return(partial_df)
  })
  
  return(full_df_reviews)
}

# ==========================================================================================

full_df <- scrape_all_reviews(agency_urls)

glimpse(full_df)

# DATA CLEANING AND PREPARATION
# Convert review_date_original to a readable date format
full_df$review_date <- lubridate::as_datetime(unlist(full_df$review_date_original))

# Unlist columns
full_df$review_title <- unlist(full_df$review_title)
full_df$review_rating <- unlist(full_df$review_rating)
full_df$review_text <- unlist(full_df$review_text)
full_df$review_verified <- unlist(full_df$review_verified)
full_df$page_number <- unlist(full_df$page_number)
full_df$agency_name <- unlist(full_df$agency_name)

# Remove leading or trailing whitespace from agency_name
full_df$agency_name <- trimws(full_df$agency_name)

# Remove review_date_original column as we have converted it to review_date
full_df <- full_df %>% select(-review_date_original)

full_df$review_rating <- as.integer(full_df$review_rating)
full_df$review_verified <- as.logical(full_df$review_verified)
full_df$page_number <- as.integer(full_df$page_number)
full_df$review_date <- as.POSIXct(full_df$review_date)

# View the modified dataframe
glimpse(full_df)

write.csv(full_df, "./data/dataframe_reviews.csv", row.names=FALSE)
