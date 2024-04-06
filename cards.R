library(rvest)   # Web scraping
library(stringr) # String manipulation
library(dplyr)   # Data manipulation

# Function to scrape data from a single review card
scrape_card <- function(url, page_num) {
  # Read the HTML content of the page
  page <- read_html(url)
  
  # Extract all review cards
  cards <- page %>% html_nodes(".styles_wrapper__2JOo2")
  
  # Initialize empty lists to store data
  agency_names <- vector("list", length = length(cards))
  categories <- vector("list", length = length(cards))
  positions <- vector("list", length = length(cards))
  
  # Extract data from each review card
  for (i in seq_along(cards)) {
    card <- cards[[i]]
    # Extract agency name
    agency_names[[i]] <- card %>% html_node(".styles_displayName__GOhL2") %>% html_text() %>% trimws()
    
    # Extract categories
    categories_list <- card %>% html_nodes(".styles_categoriesLabels__FiWQ4") %>% html_text() %>% str_split("·") %>% unlist() %>% trimws()
    categories[[i]] <- paste(categories_list, collapse = ", ")
    
    # Store position
    positions[[i]] <- i
    
  }
  
  # Combine the lists into vectors
  agency_names <- unlist(agency_names)
  categories <- unlist(categories)
  positions <- unlist(positions)
  
  # Create a data frame with the extracted data
  agency_data <- data.frame(
    page = page_num,
    position = positions,
    agency_name = agency_names,
    categories = categories
  )
  
  return(agency_data)
}

# Function to scrape all review cards from a given URL
scrape_all_cards <- function(url, base_url){
  all_agency_data <- list()  # Initialize an empty list to store data frames
  page_num <- 1  # Initialize page number
  
  # Loop to scrape all pages
  while (TRUE) {
    # Scrape data from the current page
    page_data <- scrape_card(url, page_num)
    
    # Store the data from the current page
    all_agency_data[[length(all_agency_data) + 1]] <- page_data
    
    # Check if there's a next page
    next_button <- read_html(url) %>% html_node(".pagination_pagination___F1qS .pagination-link_next__SDNU4") %>% html_attr("href")
    print(next_button) # Debugging print statement
    if (is.na(next_button)) {
      # If there's no next page, exit the loop
      break
    } else {
      # If there's a next page, update the URL to scrape the next page
      url <- paste0(base_url, next_button)
      page_num <- page_num + 1  # Increment page number
    }
  }
  
  # Combine data frames from all pages into a single data frame
  final_cards_data <- do.call(rbind, all_agency_data)
  
  return(final_cards_data)
}

# =================================================

# URL of the Trustpilot main page
home_page <- "https://it.trustpilot.com"
# URL of the Travel & Vacation category
tv_url <- paste0(home_page, "/categories/travel_vacation")

# Scrape all review cards from the specified URL
cards_scraped_df <- scrape_all_cards(tv_url, home_page)

# Print the structure of the dataframe
glimpse(cards_scraped_df)
# Save the dataframe to a CSV file
write.csv(cards_scraped_df, "./data/dataframe_cards.csv", row.names=FALSE)
