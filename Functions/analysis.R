# File: api.R
# Author: gp1981
# Purpose: Handle interactions with the Finnhub.com API

calculate_magic_formula_values <- function(extracted_data) {
  # Filter the extracted_data for the necessary variables and create new columns for calculations
  filtered_data <- extracted_data %>%
    filter(
      (financialStatement == "ic" & standard_name %in% c("Operating Income")) |
        (financialStatement == "bs" & standard_name %in% c("Total Assets", "Goodwill",
                                                           "Intangible Assets Excluding Goodwill",
                                                           "Cash and Cash Equivalents",
                                                           "Short-term investments",
                                                           "Long-term Debt",
                                                           "Accounts Payable",
                                                           "Capital Lease and Long-term Obligations"))
    )
  
  # Calculate Tangible Capital Employed by using the last quarter's Total Assets, Goodwill,
  # and Intangible Assets Excluding Goodwill
  filtered_data <- filtered_data %>%
    group_by(symbol, standard_name, financialStatement) %>%
    summarise(value = last(value)) %>%
    ungroup() %>%
    pivot_wider(names_from = standard_name, values_from = value) %>%
    mutate(
      Tangible_Capital_Employed = `Total Assets` - (`Goodwill` + `Intangible Assets Excluding Goodwill`)
    )
  
  # Calculate Enterprise Value by using the last quarter's values for Market Value of Equity,
  # Total Debt, Cash and Cash Equivalents, and Short-term investments
  filtered_data <- filtered_data %>%
    filter(standard_name %in% c("Market Value of Equity", "Total Debt",
                                "Cash and Cash Equivalents", "Short-term investments")) %>%
    group_by(symbol, financialStatement) %>%
    summarise(value = last(value)) %>%
    ungroup() %>%
    pivot_wider(names_from = standard_name, values_from = value) %>%
    mutate(
      Enterprise_Value = `Market Value of Equity` + `Total Debt` -
        (`Cash and Cash Equivalents` - `Short-term investments`)
    )
  
  # Calculate ROC and Earnings Yield by using the last 4 quarters' Operating Income
  filtered_data <- filtered_data %>%
    filter(standard_name == "Operating Income", financialStatement == "ic") %>%
    group_by(symbol) %>%
    summarise(Operating_Income = sum(value)) %>%
    mutate(
      ROC = Operating_Income / first(Tangible_Capital_Employed),
      Earnings_Yield = Operating_Income / first(Enterprise_Value)
    )
  
  # Select the relevant columns for the magic formula calculations
  magic_formula_data <- filtered_data %>%
    select(symbol, ROC, Earnings_Yield)
  
  return(magic_formula_data)
}
