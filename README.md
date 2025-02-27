# Trustpilot Data

This project consists of R scripts for scraping data from Trustpilot, particularly focusing on reviews and information related to travel and vacation agencies (italian website). It utilizes web scraping techniques with the `rvest` package, data manipulation with `dplyr`, and PostgreSQL for storing the extracted data. This work was chosen as an individual project for the Strategic and Competitive Intelligence exam of the Master of Science in Data Science and Business Informatics degree program at the University of Pisa.

## Prerequisites
- RStudio installed on your machine
- Basic knowledge of R programming language
- PostgreSQL installed and running

## Libraries
- `rvest`: For web scraping
- `dplyr`: For data manipulation
- `tidyverse`: For additional data manipulation functions
- `lubridate`: For date manipulation
- `httr`: For making HTTP requests
- `progress`: For displaying progress bars during scraping
- `RODBC`, `odbc`, `RPostgreSQL`, `RPostgres`: For PostgreSQL database connections

## How to Use
1. Open RStudio.
2. Install required packages using `install.packages(c("rvest", "dplyr", "tidyverse", "lubridate", "httr", "progress", "RODBC", "odbc", "RPostgreSQL", "RPostgres"))`.
3. Set up PostgreSQL database with the provided schema.
4. Execute each R script in the following order:
   - `cards.R`: Scrapes review cards from Trustpilot and cleans the data.
   - `agencies.R`: Scrapes information about travel and vacation agencies and cleans the data.
   - `reviews.R`: Scrapes reviews from Trustpilot for selected travel and vacation agencies, cleans the data.
   - `DB_connection.R`: Connects to the PostgreSQL database and executes SQL queries to create tables and upload data from the csv files generated by the other scripts.

## Overview of Scripts

### 1. `cards.R`
- **Functions Used**: `scrape_card`, `scrape_all_cards`
- **Outputs**:
  - `dataframe_cards.csv`: CSV file containing scraped review card data.
  - Structure of the dataframe printed to console.
- **Description**: This script scrapes review cards from Trustpilot, extracts information such as agency names and categories, and saves the data to a CSV file.

### 2. `agencies.R`
- **Functions Used**: `Rcrawler`, `map_dfr`
- **Outputs**:
  - `dataframe_agencies.csv`: CSV file containing scraped agency information.
  - Structure of the final dataframe printed to console.
- **Description**: This script scrapes information about travel and vacation agencies from Trustpilot, including website, trust score, number of reviews, claimed status, and more. It cleans and transforms the data before saving it to a CSV file.

### 3. `reviews.R`
- **Functions Used**: `scrape_reviews_singlepage`, `scrape_all_reviews`
- **Outputs**:
  - `dataframe_reviews.csv`: CSV file containing scraped review data.
  - Structure of the final dataframe printed to console.
- **Description**: This script scrapes reviews from Trustpilot for a selected list of travel agencies (first 5 agencies from `agencies` dataset). It extracts review titles, dates, ratings, texts, and verification status. The script then cleans and prepares the data before saving it to a CSV file.

### 4. `DB_connection.R`
- **Functions Used**: `dbConnect`, `dbSendQuery`, `dbExistsTable`, `dbWriteTable`
- **Outputs**: 
  - Database tables created: Cards, Agencies, Reviews
  - Success or error messages printed to console for each table creation and data insertion.
- **Description**: This script establishes a connection with a PostgreSQL database, creates tables based on predefined schemas, and inserts the scraped data into the respective tables. Error handling is implemented for table creation and data insertion.


## Database Schema
The project uses the following schema for the PostgreSQL database:
```sql
CREATE TABLE Cards (
  page INTEGER,
  position INTEGER,
  agency_name VARCHAR(255),
  categories TEXT
);

CREATE TABLE Agencies (
  website TEXT,
  trust_score REAL,
  n_reviews INTEGER,
  claim BOOLEAN,
  verified_status BOOLEAN,
  agency_name TEXT,
  info TEXT,
  page_url TEXT
);

CREATE TABLE Reviews (
  review_title TEXT,
  review_rating INTEGER,
  review_text TEXT,
  review_verified BOOLEAN,
  page_number INTEGER,
  agency_name VARCHAR(255),
  review_date DATE
);
```

## Data
- **dataframe_cards.csv**: Contains agency card data.
- **dataframe_agencies.csv**: Contains information about travel and vacation agencies.
- **dataframe_reviews.csv**: Stores scraped reviews for selected agencies.
