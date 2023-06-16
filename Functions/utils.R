# File: utils.R
# Author: gp1981
# Purpose: Utility functions for data manipulation

# Set the path to the data files
  # Set the path to the concepts directory
  conceptsDir <- "data/concepts/"
  
  # Set the file path for the standard names CSV file
  standardNamesFile <- "data/standard_names/"

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
  
  concepts<- list(balance_sheet = bsConcepts, income_statement = icConcepts, cash_flow = cfConcepts)

  bsConcepts <- concepts$balance_sheet
  icConcepts <- concepts$income_statement
  cfConcepts <- concepts$cash_flow
  
  summary_bsConcepts <- bsConcepts %>% group_by(concept) %>% distinct()
  summary_icConcepts <- icConcepts %>% group_by(concept) %>% distinct()
  summary_cfConcepts <- cfConcepts %>% group_by(concept) %>% distinct()
  
  # Save the data frames as CSV files using file.path()
  write.csv(summary_bsConcepts, file = file.path(conceptsDir, "summary_bsConcepts.csv"), row.names = FALSE)
  write.csv(summary_bsConcepts, file = file.path(conceptsDir, "summary_icConcepts.csv"), row.names = FALSE)
  write.csv(summary_bsConcepts, file = file.path(conceptsDir, "summary_cfConcepts.csv"), row.names = FALSE)
  
  return(list(summary_bsConcepts = summary_bsConcepts, summary_icConcepts = summary_icConcepts, summary_cfConcepts = summary_cfConcepts))
}

# Function to generate CSV files for balance sheet, income statement, and cash flow statement standard names
generateStandardNamesCSV <- function() {
  # Balance Sheet standard names
  balanceSheetNames <- c(
    "Cash and Cash Equivalents",
    "Short-term Investments",
    "Accounts Receivable",
    "Inventory",
    "Prepaid Expenses",
    "Property, Plant, and Equipment",
    "Intangible Assets",
    "Goodwill",
    "Long-term Investments",
    "Accounts Payable",
    "Accrued Expenses",
    "Notes Payable",
    "Deferred Revenue",
    "Long-term Debt",
    "Shareholder's Equity",
    "Retained Earnings",
    "Common Stock",
    "Treasury Stock",
    "Preferred Stock",
    "Accumulated Other Comprehensive Income"
  )
  
  # Income Statement standard names
  incomeStatementNames <- c(
    "Revenue",
    "Cost of Goods Sold",
    "Gross Profit",
    "Operating Expenses",
    "Research and Development Expenses",
    "Selling, General, and Administrative Expenses",
    "Depreciation and Amortization",
    "Operating Income",
    "Interest Expense",
    "Income Before Taxes",
    "Income Taxes",
    "Net Income",
    "Earnings Per Share (EPS)",
    "Diluted Earnings Per Share",
    "Extraordinary Items",
    "Discontinued Operations"
  )
  
  # Cash Flow Statement standard names
  cashFlowStatementNames <- c(
    "Net Income",
    "Depreciation and Amortization",
    "Deferred Taxes",
    "Stock-Based Compensation",
    "Changes in Working Capital",
    "Accounts Receivable",
    "Inventory",
    "Accounts Payable",
    "Income Taxes Payable",
    "Other Working Capital",
    "Net Cash Provided by Operating Activities",
    "Acquisition and Disposition of Property, Plant, and Equipment",
    "Purchases and Sales of Investments",
    "Issuance and Repayment of Debt",
    "Issuance and Repurchase of Stock",
    "Payment of Dividends",
    "Net Cash Provided by Financing Activities",
    "Effect of Exchange Rate Changes on Cash",
    "Net Change in Cash and Cash Equivalents"
  )
  
  # Create data frames for balance sheet, income statement, and cash flow statement names
  bsNamesDF <- data.frame(standard_names_BS = balanceSheetNames)
  icNamesDF <- data.frame(standard_names_IC = incomeStatementNames)
  cfNamesDF <- data.frame(standard_names_CF = cashFlowStatementNames)
  
  # Save data frames as CSV files
  write.csv(bsNamesDF, file = "data/standard_names/standard_names_BS.csv", row.names = FALSE)
  write.csv(icNamesDF, file = "data/standard_names/standard_names_IC.csv", row.names = FALSE)
  write.csv(cfNamesDF, file = "data/standard_names/standard_names_CF.csv", row.names = FALSE)
}

library(dplyr)
library(stringdist)

# Function to create a mapping table of concepts and standard names of BS, IC, CF
createMappingTable <- function() {
  # Load concepts from CSV files
  summary_bsConcepts <- read.csv(file.path(conceptsDir, "summary_bsConcepts.csv"))
  summary_icConcepts <- read.csv(file.path(conceptsDir, "summary_icConcepts.csv"))
  summary_cfConcepts <- read.csv(file.path(conceptsDir, "summary_cfConcepts.csv"))
  
  # Load standard names from CSV files
  standard_names_BS <- read.csv(file.path(standardNamesFile, "standard_names_BS.csv"))
  standard_names_IC <- read.csv(file.path(standardNamesFile, "standard_names_IC.csv"))
  standard_names_CF <- read.csv(file.path(standardNamesFile, "standard_names_CF.csv"))
  
  # Create mapping table
  mappingTable <- list(
    summary_bsConcepts = summary_bsConcepts,
    summary_icConcepts = summary_icConcepts,
    summary_cfConcepts = summary_cfConcepts,
    standard_names_BS = standard_names_BS,
    standard_names_IC = standard_names_IC,
    standard_names_CF = standard_names_CF
  )
  
  # Preprocess the concepts and standard names
  preprocessText <- function(text) {
    text <- tolower(text)
    text <- trimws(text)
    text <- gsub("[[:punct:]]", "", text)
    return(text)
  }
  
  # Preprocess the standard names
  standard_names_BS$standard_name <- as.character(lapply(standard_names_BS$standard_name, preprocessText))
  standard_names_IC$standard_name <- as.character(lapply(standard_names_IC$standard_name, preprocessText))
  standard_names_CF$standard_name <- as.character(lapply(standard_names_CF$standard_name, preprocessText))
  
  # Load the concepts CSV files
  bsConcepts <- read.csv(file.path(conceptsDir, "summary_bsConcepts.csv"), stringsAsFactors = FALSE)
  icConcepts <- read.csv(file.path(conceptsDir, "summary_icConcepts.csv"), stringsAsFactors = FALSE)
  cfConcepts <- read.csv(file.path(conceptsDir, "summary_cfConcepts.csv"), stringsAsFactors = FALSE)
  
  # Preprocess the concept data frames
  bsConcepts$concept <- as.character(lapply(bsConcepts$concept, preprocessText))
  icConcepts$concept <- as.character(lapply(icConcepts$concept, preprocessText))
  cfConcepts$concept <- as.character(lapply(cfConcepts$concept, preprocessText))
  
  # Initialize the mapping table
  mappingTable <- data.frame(concept = character(),
                             standard_name = character(),
                             similarity_score = numeric(),
                             stringsAsFactors = FALSE)
  
  # Function to find the best match between a concept and standard names
  findBestMatch <- function(concept, standardNames) {
    scores <- stringdist::stringdistmatrix(concept, standardNames$standard_name, method = "jw")
    maxScore <- max(scores)
    bestMatchIndices <- which(scores == maxScore, arr.ind = TRUE)
    bestMatches <- standardNames[bestMatchIndices[, "row"], ]
    bestMatches$score <- maxScore
    return(bestMatches)
  }
  
  # Iterate over the concepts and standard names
  mapConcepts <- function(concepts, standardNames) {
    mapping <- data.frame(concept = character(),
                          standard_name = character(),
                          similarity_score = numeric(),
                          stringsAsFactors = FALSE)
    
    for (i in 1:nrow(concepts)) {
      concept <- concepts[i, "concept"]
      matches <- findBestMatch(concept, standardNames)
      
      mapping <- rbind(mapping, data.frame(concept = rep(concept, nrow(matches)),
                                           standard_name = matches$standard_name,
                                           similarity_score = matches$score,
                                           stringsAsFactors = FALSE))
    }
    
    return(mapping)
  }
  
  # Map the concepts to standard names for each financial statement
  mapping_bsConcepts <- mapConcepts(bsConcepts, standard_names_BS)
  mapping_icConcepts <- mapConcepts(icConcepts, standard_names_IC)
  mapping_cfConcepts <- mapConcepts(cfConcepts, standard_names_CF)
  
  # Combine the mappings for all financial statements
  mappingTable <- rbind(mappingTable, mapping_bsConcepts, mapping_icConcepts, mapping_cfConcepts)
  
  # Sort the mapping table by concept and similarity score
  mappingTable <- mappingTable %>%
    arrange(concept, desc(similarity_score))
  
  return(mappingTable)
}
