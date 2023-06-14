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

# Function to extract concepts from financialsDF and create separate data frames
extractConcepts <- function(financialsDF) {
  bsConcepts <- data.frame()
  icConcepts <- data.frame()
  cfConcepts <- data.frame()
  
  for (i in 1:nrow(financialsDF)) {
    bs <- financialsDF[i, "report"]$bs
    ic <- financialsDF[i, "report"]$ic
    cf <- financialsDF[i, "report"]$cf
    
    bsConcepts <- bind_rows(bsConcepts, data.frame(concept = unlist(lapply(bs, function(x) x$concept))))
    icConcepts <- bind_rows(icConcepts, data.frame(concept = unlist(lapply(ic, function(x) x$concept))))
    cfConcepts <- bind_rows(cfConcepts, data.frame(concept = unlist(lapply(cf, function(x) x$concept))))
  }
  
  return(list(balance_sheet = bsConcepts, income_statement = icConcepts, cash_flow = cfConcepts))
}

# Function to map concepts to standardized labels using a mapping table
mapConcepts <- function(concepts, mappingTable) {
  mappedLabels <- character(length(concepts))
  
  for (i in seq_along(concepts)) {
    concept <- concepts[i]
    
    # Exact match
    exactMatchIndex <- grep(concept, mappingTable$Pattern, ignore.case = FALSE)
    if (length(exactMatchIndex) > 0) {
      mappedLabels[i] <- mappingTable$Label[exactMatchIndex[1]]
    } else {
      # Partial match
      partialMatchIndex <- grep(concept, mappingTable$Pattern, ignore.case = TRUE)
      if (length(partialMatchIndex) > 0) {
        mappedLabels[i] <- mappingTable$Label[partialMatchIndex[1]]
      } else {
        mappedLabels[i] <- "No Match"  # Indication for no match
      }
    }
  }
  
  return(data.frame(concept = concepts, label = mappedLabels, stringsAsFactors = FALSE))
}

