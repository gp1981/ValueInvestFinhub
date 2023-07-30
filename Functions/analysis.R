# File: api.R
# Author: gp1981
# Purpose: Handle interactions with the Finnhub.com API

calculate_magic_formula_values <- function(standardized_data) {
  # Filter data for Operating Income, Revenue, and Balance Sheet items
  relevant_data <- standardized_data %>%
    filter(standard_name %in% c("Operating Income", "Revenue", "Total Assets",
                                "Total Current Liabilities", "Intangible Assets Including Goodwill",
                                "Short-Term debt", "Long-Term debt", "Capital Lease and Obligations",
                                "Preferred Stock", "Minority Interest")) 
  
  # Extract the last 4 quarters' Operating Income for each company
  last_4_quarters_operating_income <- relevant_data %>%
    filter(standard_name == "Operating Income", financialStatement == "ic") %>%
    group_by(symbol) %>%
    slice_tail(n = 4) %>%
    mutate(Operating_Income_Last_4Q = sum(value)) %>%
    ungroup() %>%
    select(symbol, year, quarter, Operating_Income_Last_4Q, standard_name,value)
  
  # Extract the last quarter's balance sheet items for each company
  last_quarter_balance_sheet <- relevant_data %>%
    filter(financialStatement == "bs") %>%
    group_by(symbol, standard_name) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  # Calculate Excess of Cash for each company
  excess_of_cash <- last_quarter_balance_sheet %>%
    filter(standard_name == "Cash and Cash Equivalents") %>%
    pull(value) -
    0.05 * last_4_quarters_operating_income$Operating_Income_Last_4Q
  
  # Calculate Tangible Capital Employed
  tangible_capital_employed <- last_quarter_balance_sheet %>%
    filter(standard_name %in% c("Total Assets", "Intangible Assets Including Goodwill",
                                "Total Current Liabilities", "Short-Term debt")) %>%
    group_by(symbol) %>%
    summarise(
      Total_Assets = sum(value[standard_name == "Total Assets"]),
      Intangible_Assets = sum(value[standard_name == "Intangible Assets Including Goodwill"]),
      Total_Current_Liabilities = sum(value[standard_name == "Total Current Liabilities"]),
      Short_Term_Debt = sum(value[standard_name == "Short-Term debt"])
    ) %>%
    mutate(
      Tangible_Capital_Employed = Total_Assets - (Intangible_Assets + excess_of_cash +
                                                    Total_Current_Liabilities - Short_Term_Debt)
    ) %>%
    select(symbol, Tangible_Capital_Employed)
  
  # Calculate Enterprise Value
  enterprise_value <- last_quarter_balance_sheet %>%
    filter(standard_name %in% c("Market Capitalization", "Short-Term debt",
                                "Long-Term debt", "Capital Lease and Obligations",
                                "Preferred Stock", "Minority Interest")) %>%
    group_by(symbol) %>%
    summarise(
      Market_Capitalization = sum(value[standard_name == "Market Capitalization"]),
      Short_Term_Debt = sum(value[standard_name == "Short-Term debt"]),
      Long_Term_Debt = sum(value[standard_name == "Long-Term debt"]),
      Capital_Lease_Obligations = sum(value[standard_name == "Capital Lease and Obligations"]),
      Preferred_Stock = sum(value[standard_name == "Preferred Stock"]),
      Minority_Interest = sum(value[standard_name == "Minority Interest"])
    ) %>%
    mutate(
      Net_Interest_Bearing_Debt = Short_Term_Debt + Long_Term_Debt + Capital_Lease_Obligations,
      Enterprise_Value = Market_Capitalization + Net_Interest_Bearing_Debt + Preferred_Stock + Minority_Interest
    ) %>%
    select(symbol, Enterprise_Value)
  
  # Calculate ROC and Earnings Yield
  magic_formula_data <- last_4_quarters_operating_income %>%
    inner_join(tangible_capital_employed, by = "symbol") %>%
    inner_join(enterprise_value, by = "symbol") %>%
    mutate(
      ROC = Operating_Income_Last_4Q / Tangible_Capital_Employed,
      Earnings_Yield = Operating_Income_Last_4Q / Enterprise_Value
    ) %>%
    select(symbol, ROC, Earnings_Yield)
  
  return(magic_formula_data)
}
