# main.R

# Author: gp1981

# Purpose: This script replicates the ranking of companies as per "The Little Book that Still Beats the Market" by J. Greenblatt. It uses data from the Finnhub API.

# Disclaimer: The code provided here is for educational purposes only. Use it at your own risk.

# -------------------- 01 - Load Libraries --------------------
library(lubridate)
library(tidyverse)
library(tidyquant)
library(ggthemes)
library(ggplot2)
library(httr)
library(jsonlite)
library(openxlsx)

# -------------------- 02 - Source Functions --------------------
source('Functions/api.R')
source('Functions/utils.R')
source('Functions/analysis.R')

# -------------------- 03 - Data Retrieval --------------------
# Retrieve API KEY
API_KEY <- rstudioapi::askForSecret("API_KEY")

# Call the functions from api.R
stockSymbols <- getStockSymbols()
commonStocks <- filterCommonStocks(stockSymbols)

# Retrieve company profile data for common stocks
commonStockProfiles <- retrieveCompanyProfiles(commonStocks, API_KEY, maxCompanies = 30)

# Convert commonStockProfiles list to data frame
commonStockProfilesDF <- bind_rows(commonStockProfiles, .id = "symbol")

# Merge commonStocksDF and commonStockProfilesDF
commonStocksDF <- inner_join(commonStocks, commonStockProfilesDF, by = "symbol")

# Clean up unnecessary objects
rm(commonStocks, commonStockProfiles, commonStockProfilesDF)
cleanDF <- cleanCommonStocksDF(commonStocksDF)

# Filter companies based on exclusion criteria and minimum market capitalization
filteredDF <- filterCompanies(cleanDF, minMarketCapMillionUSD = 50)  # Example: Minimum market cap of 50 million USD

# Retrieve financial data for filtered companies
financialsDF <- retrieveFinancials(filteredDF, API_KEY)

concepts <- extractConcepts(financialsDF)
bsConcepts <- concepts$balance_sheet
icConcepts <- concepts$income_statement
cfConcepts <- concepts$cash_flow

# Read the mapping table from the CSV file
mappingTable <- read.csv("Functions/mapping_table.csv")

# Map bsConcepts, icConcepts, and cfConcepts using the mapping table
bsMapped <- mapConcepts(bsConcepts$concept, mappingTable) #-> still pattern=goodwill is labelled "intangibles_less_goodwill"
icMapped <- mapConcepts(icConcepts$concept, mappingTable)
cfMapped <- mapConcepts(cfConcepts$concept, mappingTable)
# -------------------- 04 - Data Analysis --------------------
# Perform analysis on common stocks data frame
# Call functions from analysis.R or add your analysis code here

# -------------------- 05 - Data Visualization --------------------
# Create visualizations based on the analysis results
# Add your code for generating visualizations using ggplot2 or other libraries here

# -------------------- 06 - Output or Export Results --------------------
# Save the analysis results or export data frame to a file
# Add your code for saving/exporting the results here

# -------------------- 07 - Print or Display Results --------------------
# Print or display the analysis results
print(commonStocksDF)
