# File: api.R
# Author: gp1981
# Purpose: Handle interactions with the Finnhub.com API

# Calculate the values for Greenblatt's Magic Formula variables
calculate_magic_formula_values <- function(extracted_data) {
  # Filter the extracted_data dataframe for the necessary variables
  filtered_data <- extracted_data %>%
    filter(standard_name %in% c("Operating Income", "Total Assets", "goodwillAndIntangibleAssets",
                                "cashAndShortTermInvestments", "Total Debt", "capitalLeaseObligations",
                                "Share Price", "Outstanding Common Shares", "minority interest",
                                "preferred stock"))
  
  # Separate the balance sheet and income statement items
  bs_data <- filtered_data %>% filter(financialStatement == "bs")
  ic_data <- filtered_data %>% filter(financialStatement == "ic")
  
  # Aggregate the values for balance sheet items (taking only the last quarter)
  bs_data <- bs_data %>%
    group_by(symbol) %>%
    arrange(desc(year), desc(quarter)) %>%
    slice(1) %>%
    ungroup()
  
  # Aggregate the values for income statement items (taking the last 4 quarters)
  ic_data <- ic_data %>%
    group_by(symbol) %>%
    arrange(desc(year), desc(quarter)) %>%
    mutate(row_number = row_number()) %>%
    filter(row_number <= 4) %>%
    ungroup()
  
  # Manipulate the values to obtain the necessary variables for the magic formula
  bs_data <- bs_data %>%
    mutate(
      Tangible_Capital_Employed = `Total Assets` - (`goodwillAndIntangibleAssets` + `cashAndShortTermInvestments`)
    )
  
  ic_data <- ic_data %>%
    mutate(
      Enterprise_Value = (`Share Price` * `Outstanding Common Shares`) +
        `Total Debt` + `capitalLeaseObligations` - (`cashAndShortTermInvestments` - `Cash&CashEquivalent`)
    )
  
  # Calculate the variables in the magic formula
  bs_data <- bs_data %>%
    mutate(
      ROC = `Operating Income` / `Tangible_Capital_Employed`
    )
  
  ic_data <- ic_data %>%
    mutate(
      Earnings_Yield = `Operating Income` / `Enterprise_Value`
    )
  
  # Create a new dataframe with the calculated values
  calculated_values <- data.frame(
    symbol = unique(filtered_data$symbol),
    ROC = bs_data$ROC,
    Earnings_Yield = ic_data$Earnings_Yield
  )
  
  return(calculated_values)
}
