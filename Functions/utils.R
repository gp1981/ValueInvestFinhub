# File: utils.R
# Author: gp1981
# Purpose: Utility functions for data manipulation

# Function to create a pivot table of industries in commonStocksDF
createIndustryPivot <- function(commonStocksDF) {
  industryPivot <- table(commonStocksDF$FinnhubIndustry)
  return(industryPivot)
}

# Function to exclude companies from commonStocksDF based on industry names
excludeCompanies <- function(commonStocksDF, excludedIndustries) {
  filteredStocks <- commonStocksDF[!commonStocksDF$FinnhubIndustry %in% excludedIndustries, ]
  return(filteredStocks)
}


# Function to clean up commonStocksDF
cleanCommonStocksDF <- function(commonStocksDF) {
  cleanedDF <- commonStocksDF %>%
    select(-c("currency.y", "estimateCurrency", "symbol2", "displaySymbol", "ticker", "isin")) %>%
    distinct() %>%
    rename(currency = currency.x)
  
  return(cleanedDF)
}

# Function to filter companies based on exclusion criteria and minimum market capitalization
filterCompanies <- function(df, excludedIndustries = c("Utilities", "Banking", "Insurance"), allowedCountries = c("US", "CA"), minMarketCapMillionUSD = 0) {
  filteredDF <- df %>%
    filter(!finnhubIndustry %in% excludedIndustries) %>%
    filter(country %in% allowedCountries) %>%
    filter(marketCapitalization >= minMarketCapMillionUSD)
  
  return(filteredDF)
}