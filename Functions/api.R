# File: api.R
# Author: gp1981 with the contribution of ChatGPT4.0
# Purpose: Handle interactions with the Finnhub.com API

# api.R

# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(progress)

# Function to retrieve stock symbols from Finnhub API
getStockSymbols <- function() {
  url <- paste0("https://finnhub.io/api/v1/stock/symbol?exchange=US&token=", API_KEY)
  response <- GET(url)
  data <- content(response, as = "text")
  stockSymbols <- fromJSON(data)
  return(stockSymbols)
}

# Main function to filter common stocks from stock symbols
filterCommonStocks <- function(stockSymbols) {
  symbols_df <- as.data.frame(stockSymbols)
  commonStocks <- symbols_df %>% filter(type == "Common Stock")
  return(commonStocks)
}

# Function to retrieve company profile data for a stock symbol
getCompanyProfile <- function(symbol, apiKey) {
  url <- paste0("https://finnhub.io/api/v1/stock/profile2?symbol=", symbol, "&token=", apiKey)
  response <- GET(url)
  data <- content(response, as = "text")
  profile <- fromJSON(data)
  return(profile)
}


# Retrieve company profile data for randomly selected common stocks
retrieveCompanyProfiles <- function(commonStocksDF, apiKey, maxCompanies = 300) {
  selectedStocksDF <- commonStocksDF %>% sample_n(min(maxCompanies, nrow(.)))
  
  pb <- progress_bar$new(
    format = "[:bar] :percent Elapsed: :elapsed ETA: :eta",
    total = nrow(selectedStocksDF)
    )
  profiles <- list()
  for (i in 1:nrow(selectedStocksDF)) {
    symbol <- selectedStocksDF$symbol[i]
    profile <- getCompanyProfile(symbol, apiKey)
    profiles[[symbol]] <- profile
    pb$tick()
    
    # Delay between API calls to comply with the rate limit
    if (i < nrow(selectedStocksDF)) {
      Sys.sleep(1)  # Adjust the sleep duration as needed
    }
  }
  return(profiles)
}

# Function to retrieve financial data for filtered companies from the Finnhub API
retrieveFinancials <- function(filteredDF, apiKey) {
  totalCompanies <- nrow(filteredDF)
  progress <- progress::progress_bar$new(
    format = "[:bar] :percent Elapsed: :elapsed ETA: :eta",
    total = totalCompanies
  )
  
  financialsDF <- data.frame()
  
  for (i in 1:totalCompanies) {
    symbol <- filteredDF[i, "symbol"]
    url <- paste0("https://finnhub.io/api/v1/stock/financials-reported?symbol=", symbol, "&freq=quarterly&token=", apiKey)
    response <- httr::GET(url)
    data <- httr::content(response, as = "text", encoding = "UTF-8")
    
    financials <- jsonlite::fromJSON(data)$data
    financialsDF <- dplyr::bind_rows(financialsDF, as.data.frame(financials))
    
    progress$tick()
    
    # Delay between API calls to comply with the rate limit
    if (i < nrow(filteredDF)) {
      Sys.sleep(1)  # Adjust the sleep duration as needed
    }
  }
  
  return(financialsDF)
}