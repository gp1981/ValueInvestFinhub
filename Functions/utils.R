# File: utils.R
# Author: gp1981 with the contribution of ChatGPT4.0
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
  
  summary_bsConcepts <- bsConcepts %>% group_by(concept) %>% dplyr::summarise(n = n()) %>% 
    mutate(percentage = n / sum(n) * 100) %>% arrange(desc(percentage)) %>% mutate(cumulative_percentage = cumsum(percentage))
  summary_icConcepts <- icConcepts %>% group_by(concept) %>% dplyr::summarise(n = n()) %>% 
    mutate(percentage = n / sum(n) * 100) %>% arrange(desc(percentage)) %>% mutate(cumulative_percentage = cumsum(percentage))
  summary_cfConcepts <- cfConcepts %>% group_by(concept) %>% dplyr::summarise(n = n()) %>% 
    mutate(percentage = n / sum(n) * 100) %>% arrange(desc(percentage)) %>% mutate(cumulative_percentage = cumsum(percentage))
  
  # Save the data frames as CSV files using file.path()
  write.csv(summary_bsConcepts, file = file.path(conceptsDir, "summary_bsConcepts.csv"), row.names = FALSE)
  write.csv(summary_icConcepts, file = file.path(conceptsDir, "summary_icConcepts.csv"), row.names = FALSE)
  write.csv(summary_cfConcepts, file = file.path(conceptsDir, "summary_cfConcepts.csv"), row.names = FALSE)
  
  return(list(summary_bsConcepts = summary_bsConcepts, summary_icConcepts = summary_icConcepts, summary_cfConcepts = summary_cfConcepts))
}

# Function to generate CSV files for balance sheet, income statement, and cash flow statement standard names
generateStandardNamesCSV <- function() {
  # Balance Sheet standard names
  balanceSheetNames <- c(
    "Cash and Cash Equivalents",
    "Short-term Investments",
    "Cash and Cash Equivalents and Short-term Investments",
    "Accounts Receivable",
    "Inventory",
    "Prepaid Expenses",
    "Other Current Assets",
    "Total Current Assets",
    "Gross Property, Plant and Equipment",
    "Accumulated depreciation and amortization",
    "Net Property, Plant, and Equipment, after depreciation and amortization",
    "Total Intangible Assets",
    "Goodwill",
    "Intangible Assets Excluding Goodwill",
    "Other Long-term Assets",
    "Total Assets",
    "Accounts Payable",
    "Accrued Expenses",
    "Taxes Payable",
    "Notes Payable",
    "Deferred Taxes",
    "Deferred Revenue",
    "Current Capital Lease and Long-term Obligations",
    "Total Current Liabilities",
    "Capital Lease and Obligations",
    "Long-term Debt",
    "Pension and Other Post-retirement Benefits",
    "Total Liabilities",
    "Shareholders' Equity",
    "Retained Earnings",
    "Common Stock",
    "Additional Paid-In Capital",
    "Treasury Stock",
    "Preferred Stock"
  )
  
  # Income Statement standard names
  incomeStatementNames <- c(
    "Revenue",
    "Cost of Goods Sold",
    "Gross Profit",
    "Research and Development Expenses",
    "Selling, General, and Administrative Expenses",
    "Depreciation and Amortization",
    "Operating Income",
    "Interest Expense",
    "Income Before Taxes",
    "Income Taxes, tax provision",
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
    "Increase in Working Capital",
    "Decrease in Working Capital",
    "Change in Net Working Capital",
    "Increase in Accounts Receivable",
    "Decrease in Accounts Receivable",
    "Change in Net Accounts Receivable",
    "Increase in Inventory",
    "Decrease in Inventory",
    "Change in Net Inventory",
    "Increase in Accounts Payable",
    "Decrease in Accounts Payable",
    "Change in Net Accounts Payable",
    "Income Taxes Payable",
    "Other Working Capital",
    "Cash Flow from Others",
    "Net Cash Provided by Operating Activities",
    "Acquisition of Property, Plant, and Equipment",
    "Disposition of Property, Plant, and Equipment",
    "Purchases of Investments",
    "Sales of Investments",
    "Payment of Debt",
    "Issuance of Debt",
    "Issuance of Stock",
    "Repurchase of Stock",
    "Payment of Dividends",
    "Net Cash Provided by Financing Activities",
    "Effect of Exchange Rate Changes on Cash",
    "Net Change in Cash and Cash Equivalents"
  )
  
  # Create data frames for balance sheet, income statement, and cash flow statement names
  bsNamesDF <- data.frame(standard_name = balanceSheetNames)
  icNamesDF <- data.frame(standard_name = incomeStatementNames)
  cfNamesDF <- data.frame(standard_name = cashFlowStatementNames)
  
  # Save data frames as CSV files
  write.csv(bsNamesDF, file = "data/standard_names/standard_names_BS.csv", row.names = FALSE)
  write.csv(icNamesDF, file = "data/standard_names/standard_names_IC.csv", row.names = FALSE)
  write.csv(cfNamesDF, file = "data/standard_names/standard_names_CF.csv", row.names = FALSE)
}


library(dplyr)
library(stringdist)
library(stringr)

# Function to create a mapping table of concepts and standard names of BS, IC, CF
createMappingTable <- function() {
  # Load the standard names CSV files
  bsNamesDF <- read.csv("data/standard_names/standard_names_BS.csv", stringsAsFactors = FALSE)
  icNamesDF <- read.csv("data/standard_names/standard_names_IC.csv", stringsAsFactors = FALSE)
  cfNamesDF <- read.csv("data/standard_names/standard_names_CF.csv", stringsAsFactors = FALSE)
  
  # Extract the standard names from the data frames
  standard_names_BS <- bsNamesDF$standard_name
  standard_names_IC <- icNamesDF$standard_name
  standard_names_CF <- cfNamesDF$standard_name
  
  # Function to split words and remove prefixes
  split_words <- function(input_string, split_chars) {
    # Create a pattern to match any of the split_chars
    split_pattern <- paste0("[", paste0(split_chars, collapse = "|"), "]")
    
    # Replace split_chars with a space
    cleaned_string <- gsub(split_pattern, " ", input_string)
    
    # Split the cleaned_string into words
    words <- str_split(cleaned_string, "(?<=.)(?=[A-Z][a-z])", simplify = TRUE)
    
    # Return the words
    return(words)
  }
  
  # Function to map concepts to standard names
  mapConcepts <- function(concepts, standard_names) {
    mapping <- data.frame(concept = character(0), standard_name = character(0), stringsAsFactors = FALSE)
    
    for (concept in concepts) {
      # Remove prefixes from concept
      concept_cleaned <- gsub("^[^_: ]*[:_ ]", "", concept)
      
      # Split concept into words
      concept_words <- str_split(concept_cleaned, "\\W+")[[1]]
      
      # Calculate string distances between concept words and standard names
      distances <- stringdist::stringdistmatrix(concept_words, standard_names)
      
      # Find the minimum distance and its corresponding standard name
      min_distances <- apply(distances, 1, min)
      match <- standard_names[apply(distances, 1, which.min)]
      
      # Add the mapping to the data frame
      mapping <- rbind(mapping, data.frame(concept = concept, standard_name = match, stringsAsFactors = FALSE))
    }
    
    return(mapping)
  }
  
  # Map concepts to standard names for balance sheet, income statement, and cash flow statement
  mappingTable <- list(
    bs = mapConcepts(summary_concepts$summary_bsConcepts, standard_names_BS),
    ic = mapConcepts(summary_concepts$summary_icConcepts, standard_names_IC),
    cf = mapConcepts(summary_concepts$summary_cfConcepts, standard_names_CF)
  )
  
  return(mappingTable)
}

get_symbols_by_concept <- function(type_of_statement, concept_description, year, quarter) {
  # Filter the financialsDF based on year and quarter
  filtered_df <- financialsDF[financialsDF$year == year & financialsDF$quarter == quarter, ]
  
  # Retrieve the specified type of statement
  statement <- filtered_df$report[[type_of_statement]]
  
  # Filter the statement based on concept description
  filtered_statement <- statement[sapply(statement, function(x) concept_description %in% x$concept)]
  
  # Extract the symbols from the filtered statement
  symbols <- filtered_df$symbol
  
  # Return the symbols
  symbols <- as.data.frame(symbols)
}


