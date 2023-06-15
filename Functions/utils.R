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

# Function to initialize the mapping table
initializeMapping <- function(concepts) {
  bsConcepts <- concepts$balance_sheet
  icConcepts <- concepts$income_statement
  cfConcepts <- concepts$cash_flow
  
  summary_bsConcepts <- bsConcepts %>% group_by(concept) %>% distinct()
  summary_icConcepts <- icConcepts %>% group_by(concept) %>% distinct()
  summary_cfConcepts <- cfConcepts %>% group_by(concept) %>% distinct()
  
  return(list(summary_bsConcepts  = summary_bsConcepts , summary_icConcepts = summary_icConcepts, summary_cfConcepts = summary_cfConcepts))
}

# Function to map the concepts string of the financial data
map_concepts <- function(mappingTable) {
 
  # Retrieve the dataframe of the distinctive concepts
  summary_bsConcept <- mappingTable$summary_bsConcepts
  summary_icConcept <- mappingTable$summary_icConcepts
  summary_cfConcept <- mappingTable$summary_cfConcepts
  
  # Save the data frames as CSV files
  write.csv(summary_bsConcept, file = "summary_bsConcept.csv", row.names = FALSE)
  write.csv(summary_icConcept, file = "summary_icConcept.csv", row.names = FALSE)
  write.csv(summary_cfConcept, file = "summary_cfConcept.csv", row.names = FALSE)
  
  # ..remaining code...
  return()
}

