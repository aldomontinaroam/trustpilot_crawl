library(DBI) # Database interface
library (RODBC) # ODBC connection
library(odbc) # ODBC connection
library(dplyr) # Data manipulation
library(dbplyr) # Database manipulation
library(RPostgreSQL) # PostgreSQL connection
library(RPostgres) # PostgreSQL connection

# Connect to the database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "trustpilot_sci",
  user = "postgres",
  password = rstudioapi::askForPassword("Database password")
  )

# Load data frames
dataframe_cards <- read.csv("./data/dataframe_cards.csv")
dataframe_agencies <- read.csv("./data/dataframe_agencies.csv")
dataframe_reviews <- read.csv("./data/dataframe_reviews.csv")


# Print structure of data frames
for (df in list(dataframe_cards, dataframe_agencies, dataframe_reviews)) {
  print(summary(df))
  print(str(df))
}

# Define table creation queries
table_queries <- c(
  "CREATE TABLE Cards (
     page INTEGER,
     position INTEGER,
     agency_name VARCHAR(255),
     categories TEXT
   );",
  "CREATE TABLE Agencies (
    website TEXT,
    trust_score REAL,
    n_reviews INTEGER,
    claim BOOLEAN,
    verified_status BOOLEAN,
    agency_name TEXT,
    info TEXT,
    page_url TEXT
    );",
  "CREATE TABLE Reviews (
     review_title TEXT,
     review_rating INTEGER,
     review_text TEXT,
     review_verified BOOLEAN,
     page_number INTEGER,
     agency_name VARCHAR(255),
     review_date DATE
   );"
)

# Execute table creation queries with error handling
for (query in table_queries) {
  result <- try(dbSendQuery(con, query), silent = FALSE)
  if (inherits(result, "try-error")) {
    cat("Error creating table:", class(result), "\n")
  } else {
    cat("Table created successfully\n")
  }
}

# Function to insert data into the tables
insert_data <- function(table_name, dataframe) {
  # Check if the table already exists
  if (dbExistsTable(con, table_name)) {
    # Append data to the existing table
    tryCatch({
      dbWriteTable(con, table_name, dataframe, append = TRUE)
      cat("Data inserted into", table_name, "table successfully\n")
    }, error = function(e) {
      cat("Error inserting data into", table_name, "table:", conditionMessage(e), "\n")
    })
  } else {
    cat("Table", table_name, "does not exist\n")
  }
}

# Insert data into the tables
insert_data("cards", dataframe_cards)
insert_data("reviews", dataframe_reviews)
insert_data("agencies", dataframe_agencies)

# Disconnect from the database
dbDisconnect(con)
