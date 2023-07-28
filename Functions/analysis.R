# File: api.R
# Author: gp1981
# Purpose: Handle interactions with the Finnhub.com API

calculate_magic_formula_values <- function(extracted_data) {
  # Filter the extracted_data for the necessary variables and create new columns for calculations
  filtered_data <- extracted_data %>%
    filter(
      (financialStatement == "ic" & standard_name %in% c("Operating Income (Loss)", 
                                                         "Income (Loss) from Continuing Operations before Income Taxes, Extraordinary Items, Noncontrolling Interest")) |
        (financialStatement == "bs" & standard_name %in% c("Total assets", "Goodwill",
                                                           "Intangible Assets, Net (Excluding Goodwill)",
                                                           "Cash and cash equivalents",
                                                           "Short-term investments",
                                                           "Long-term Debt","Long-term debt, net of current portion","Long-term debt",
                                                           "Accounts Payable","Accounts Payable and Accrued Liabilities, Current",
                                                           "Accounts payable to related parties",
                                                           "Accounts Payable and Accrued Expenses Related Party Current",
                                                           "Capital Lease and Long-term Obligations", "Long-term Debt and Capital Lease Obligations, Current",
                                                           "Capital Lease Obligations, Current","Capital leases","CAPITAL LEASE OBLIGATIONS",
                                                           "Capital Lease Obligations, Noncurrent"))
    )
  
  # Add the harmonized_standard_name based on conditions
  harmonized_filtered_data <- filtered_data %>%
    mutate(
      harmonized_standard_name = case_when(
        standard_name %in% c("Operating Income (Loss)", 
                             "Income (Loss) from Continuing Operations before Income Taxes, Extraordinary Items, Noncontrolling Interest") ~ "Operating Income",
        standard_name %in% c("Long-term Debt","Long-term debt, net of current portion","Long-term debt") ~ "Long-Term debt",
        standard_name %in% c("Accounts Payable","Accounts Payable and Accrued Liabilities, Current",
                             "Accounts payable to related parties", "Accounts Payable and Accrued Expenses Related Party Current") ~ "Account Payable",
        standard_name %in% c("Capital Lease and Long-term Obligations", "Long-term Debt and Capital Lease Obligations, Current",
                             "Capital Lease Obligations, Current","Capital leases","CAPITAL LEASE OBLIGATIONS", "Capital Lease Obligations, Noncurrent") ~ "Capital Lease and Obligations",
        TRUE ~ standard_name  # For other cases, keep the original standard_name
      )
    ) %>% select(harmonized_standard_name,everything())
  
  # Semi-join with filtereDF data frame to add marketCapitalization
  harmonized_filtered_data <- semi_join(harmonized_filtered_data, filteredDF, by = "symbol") #<<<- somehow does not work
  
  # Calculate Tangible Capital Employed by using the last quarter's Total Assets, Goodwill,
  # and Intangible Assets Excluding Goodwill
  filtered_data <- harmonized_filtered_data %>%
    group_by(symbol, harmonized_standard_name, financialStatement) %>%
    summarise(value = last(value)) %>%
    ungroup() %>%
    pivot_wider(names_from = harmonized_standard_name, values_from = value) %>%
    mutate(
      Tangible_Capital_Employed = `Total assets` - (`Goodwill` + `Intangible Assets, Net (Excluding Goodwill)`)
    )
  

  # Calculate Enterprise Value by using the last quarter's values for Market Value of Equity,
  # Total Debt, Cash and Cash Equivalents, and Short-term investments
  filtered_data2 <- harmonized_filtered_data %>%
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
