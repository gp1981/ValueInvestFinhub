# File: utils.R
# Author: gp1981 with the contribution of ChatGPT4.0
# Purpose: Utility functions for data manipulation

# Load Libraries
library(dplyr)
library(stringdist)
library(stringr)
library(progress)
library(janitor)
library(tidyr)
library(zoo)

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
filterCompanies <- function(df, excludedIndustries = c("Utilities", "Banking", "Insurance", "Financial Services"), 
                            allowedCountries = c("US", "CA"), minMarketCapMillionUSD = 0) {
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

# Extract the list of symbols and last financial statement providing type of statement, concept, year and quarter
extract_financials_data <- function(mapping_table_path, financials_data) {
  
  # Create an empty data frame to store the extracted data
  extracted_data <- data.frame(label = character(),
                               unit = character(),
                               value = integer(),
                               symbol = character(),
                               year = integer(),
                               quarter = integer(),
                               filedDate = character(),
                               stringsAsFactors = FALSE)
  
  # Load the mapping table to get standardised labels
  mappingTable <- read.csv("data/mappingTable.csv")  # Adjust the file path if needed
  
  
  # # Define the number of iterations for the loop
  total_iterations <- length(financials_data$report$bs)
  # 
  # # Initialize the outer progress bar
  pb <- progress_bar$new(format = "[:bar] :percent Elapsed: :elapsed ETA: :eta",
                         total = total_iterations)
  
  
  # Initialize empty data frames for extracted_data, bs_data, ic_data, and cf_data
  extracted_data <- data.frame(label = character(),
                               unit = character(),
                               value = double(),
                               concept = character(),
                               symbol = character(),
                               year = integer(),
                               quarter = integer(),
                               filedDate = character(),
                               stringsAsFactors = FALSE)
  bs_data <- data.frame(label = character(),
                        unit = character(),
                        value = double(),
                        concept = character(),
                        symbol = character(),
                        year = integer(),
                        quarter = integer(),
                        filedDate = character(),
                        stringsAsFactors = FALSE)
  ic_data <- data.frame(label = character(),
                        unit = character(),
                        value = double(),
                        concept = character(),
                        symbol = character(),
                        year = integer(),
                        quarter = integer(),
                        filedDate = character(),
                        stringsAsFactors = FALSE)
  cf_data <- data.frame(label = character(),
                        unit = character(),
                        value = double(),
                        concept = character(),
                        symbol = character(),
                        year = integer(),
                        quarter = integer(),
                        filedDate = character(),
                        stringsAsFactors = FALSE)
  
  # Iterate over each symbol, year, and quarter
  for (i in 1:length(financials_data$report$bs)) {
    pb$tick()
    
    # Extract the financials data for the current symbol, year, and quarter
    if (length(financials_data$report$bs[[i]]) > 0) {
      bs_data <- financials_data$report$bs[[i]] %>% 
        mutate(across(c(label, unit, concept), as.character), across(value, as.double)) %>%
        select(label, unit, value, concept) %>% clean_names() %>%
        mutate(symbol = financials_data$symbol[i],
               year = as.integer(financials_data$year[i]),
               quarter = as.integer(financials_data$quarter[i]),
               filedDate = financials_data$filedDate[i],
               financialStatement = "bs")
      
      extracted_data <- bind_rows(extracted_data, bs_data)
    }
    
    if (length(financials_data$report$ic[[i]]) > 0) {
      ic_data <- financials_data$report$ic[[i]] %>% 
        mutate(across(c(label, unit, concept), as.character), across(value, as.double)) %>%
        select(label, unit, value, concept) %>% clean_names() %>%
        mutate(symbol = financials_data$symbol[i],
               year = as.integer(financials_data$year[i]),
               quarter = as.integer(financials_data$quarter[i]),
               filedDate = financials_data$filedDate[i],
               financialStatement = "ic")
      
      extracted_data <- bind_rows(extracted_data, ic_data)
    }
    
    if (length(financials_data$report$cf[[i]]) > 0) {
      cf_data <- financials_data$report$cf[[i]] %>% 
        mutate(across(c(label, unit, concept), as.character), across(value, as.double)) %>%
        select(label, unit, value, concept) %>% clean_names() %>%
        mutate(symbol = financials_data$symbol[i],
               year = as.integer(financials_data$year[i]),
               quarter = as.integer(financials_data$quarter[i]),
               filedDate = financials_data$filedDate[i],
               financialStatement = "cf")
      
      extracted_data <- bind_rows(extracted_data, cf_data)
    }
  }
  
  
  # Perform the join with mapping_table to fill in the standard_name
  extracted_data_std <- extracted_data %>%
    left_join(mappingTable, by = "concept") %>%
    mutate(standard_name = ifelse(is.na(standard_name), label, standard_name))
  
  # Remove the duplicated rows based on label,unit,value,and concept != NA
  extracted_data_std <- extracted_data_std %>%  
    distinct(standard_name,unit,value,.keep_all = TRUE) %>% 
    select(standard_name,unit,value, everything())
  
  # Clean data set from rows with (all) NA
  cleaned_data <- na.omit(extracted_data)
  
  # Return the extracted_data data frame
  return(extracted_data_std)
}

# Get unique values from extracted_data DataFrame
# Returns: all_companies - a vector of unique company symbols
#          all_years_list - a vector of unique years
#          all_quarters_list - a vector of unique quarters
get_unique_values <- function(extracted_data) {
  # Get unique companies
  all_companies <- unique(extracted_data$symbol)
  
  # Get unique years
  all_years_list <- unique(extracted_data$year)
  
  # Get unique quarters
  all_quarters_list <- unique(extracted_data$quarter)
  
  # Return the lists
  return(list(all_companies = all_companies,
              all_years_list = all_years_list,
              all_quarters_list = all_quarters_list))
}

