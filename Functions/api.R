# File: api.R
# Author: gp1981
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
  
  pb <- progress_bar$new(total = nrow(selectedStocksDF))
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



