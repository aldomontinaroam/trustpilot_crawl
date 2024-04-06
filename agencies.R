library(tidyverse) # Data manipulation
library(Rcrawler) # Web scraping
library(dplyr) # Data manipulation
library(tidyr) # Data manipulation

# Fetch the Travel & Vacation category page
page <- read_html("https://it.trustpilot.com/categories/travel_vacation")

# Extract the last page number
last_page <- page %>%
  html_nodes(".pagination_pagination___F1qS .pagination-link_item__mkuN3") %>%
  html_text() %>%
  as.integer() %>%
  max()
# Set MaxDepth based on the last page number
max_depth <- last_page

# Crawl the Travel & Vacation category page and extract the URLs of individual agency pages
Rcrawler(Website = "https://it.trustpilot.com/categories/travel_vacation",
         crawlUrlfilter = "/categories/travel_vacation\\?page=[0-9]+$", 
         no_cores = 4,
         no_conn = 4,
         MaxDepth = max_depth, # Use max_depth in Rcrawler function
         saveOnDisk = FALSE)

url_list <- INDEX$Url # Get the list of URLs

# Crawl individual agency pages and extract relevant information
agencies_df <- map_dfr(url_list, ~{
  Rcrawler(Website = .,
           crawlUrlfilter = "/review/[^/]+$",
           dataUrlfilter = "/review/[^/]+$",
           crawlZoneXPath = "//div[@class='paper_paper__1PY90 paper_outline__lwsUX card_card__lQWDv card_noPadding__D8PcU styles_wrapper__2JOo2']",
           ExtractXpathPat = c("//div[@class='styles_cardBadge__LeaaQ']/a/@href", # website
                               "//div[@class='styles_rating__uyC6m styles_clickable__uWsnU styles_rating__NPyeH']/p", # trustscore
                               "//p[@class='typography_body-l__KUYFJ typography_appearance-default__AAY17']/text()", # number of reviews
                               "//div[@class='styles_listItem__7beWu']/span/button/span", # claimed or not
                               "//div[@class='typography_body-xs__FxlLP typography_appearance-default__AAY17 typography_weight-heavy__E1LTj styles_verificationIcon___X7KO']", # verified status
                               "//span[@class='typography_display-s__qOjh6 typography_appearance-default__AAY17 title_displayName__TtDDM']/text()", # agency name
                               "//div[@class='styles_container__9nZxD customer-generated-content']/text()", # agency info
                               "/html/head/link[@rel='canonical']/@href" # review page URL
           ),
           PatternsNames = c("website", "trust_score", "n_reviews", "claim", "verified_status", "agency_name", "info", "page_url"),
           no_cores = 4,
           no_conn = 4,
           MaxDepth = 10,
           RequestsDelay = 0.5,
           saveOnDisk = FALSE)
  
  df <- do.call("rbind", DATA) %>%
    data.frame()
  
  # Convert claimed or not to boolean
  df$claim <- ifelse(df$claim == "Profilo reclamato", TRUE, FALSE)
  
  # Convert verified or not to boolean
  df$verified_status <- ifelse(df$verified_status == "AZIENDA VERIFICATA", TRUE, FALSE)
  
  return(df)
})

# Print the structure of the final dataframe
str(agencies_df)
glimpse(agencies_df)

# Data cleaning and transformation
agencies_df <- select(agencies_df, -PageID) # Remove PageID column
glimpse(agencies_df)

agencies_df$trust_score[is.na(agencies_df$trust_score)] <- 0 # Replace NA with 0
agencies_df$trust_score <- as.numeric(gsub(",", ".", agencies_df$trust_score)) # Replace comma with dot
agencies_df$trust_score <- round(agencies_df$trust_score, 1) # Round to 1 decimal
glimpse(agencies_df)

agencies_df$n_reviews <- as.numeric(gsub("\\.", "", agencies_df$n_reviews)) # Remove dots
agencies_df$n_reviews[is.na(agencies_df$n_reviews)] <- 0 # Replace NA with 0
agencies_df$n_reviews <- as.integer(agencies_df$n_reviews) # Convert to integer
glimpse(agencies_df)

agencies_df$claim <- as.logical(agencies_df$claim) # Convert to boolean
glimpse(agencies_df)

agencies_df$verified_status <- as.logical(agencies_df$verified_status) # Convert to boolean
agencies_df$verified_status <- ifelse(is.na(agencies_df$verified_status), FALSE, as.logical(agencies_df$verified_status)) # Convert NA to FALSE
glimpse(agencies_df)

colnames(agencies_df) <- tolower(colnames(agencies_df)) # Convert column names to lowercase
glimpse(agencies_df)
head(agencies_df)

# Convert <list> to <strings>
agencies_df <- agencies_df %>%
  mutate_at(vars(website, agency_name, info, page_url), ~ ifelse(sapply(., is.list), .[[1]], .)) %>%
  unnest(cols = c(website, agency_name, info, page_url))

glimpse(agencies_df)


# Save the dataframe to a CSV file
write.csv(agencies_df, "./data/dataframe_agencies.csv", row.names=FALSE)







# ====================================

# Ignore the following code (just for testing purposes)


# # Example of ContentScraper
# ContentScraper(Url = "https://it.trustpilot.com/review/crocierepiu.it",
#                XpathPatterns = c("//div[@class='styles_cardBadge__LeaaQ']/a/@href", # website
#                                  "//div[@class='styles_rating__uyC6m styles_clickable__uWsnU styles_rating__NPyeH']/p", # trustscore
#                                  "//p[@class='typography_body-l__KUYFJ typography_appearance-default__AAY17']/text()", # number of reviews
#                                  "//div[@class='styles_listItem__7beWu']/span/button/span", # claimed or not
#                                  "//div[@class='typography_body-xs__FxlLP typography_appearance-default__AAY17 typography_weight-heavy__E1LTj styles_verificationIcon___X7KO']", # verified status
#                                  "//span[@class='typography_display-s__qOjh6 typography_appearance-default__AAY17 title_displayName__TtDDM']/text()", # agency name
#                                  "//div[@class='styles_container__9nZxD customer-generated-content']/text()"
#                )
# )
