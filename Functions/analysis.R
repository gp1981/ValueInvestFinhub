# File: api.R
# Author: gp1981
# Purpose: Handle interactions with the Finnhub.com API

calculate_magic_formula_values <- function(extracted_data) {
  # Standardize further standard_name based on conditions
  filtered_data <- extracted_data %>%
    mutate(
      standard_name = case_when(
        # Operating Income
        standard_name %in% c("Operating income", "Income from operations", "Income before income taxes",
                             "Income Before Income Taxes", "Income from continuing operations",
                             "Income from continuing operations before taxes", "Income from operations",
                             "LOSS BEFORE INCOME TAXES", "Loss From Operations Before Income Taxes", 
                             "Other Income / Loss") ~ "Operating Income",
        
        # Total Assets
        standard_name %in% c("Total assets") ~ "Total Assets",
        
        # Intangible Assets
        standard_name %in% c("Intangible and other assets", "Goodwill and Intangible Assets Net", 
                             "Goodwill and other intangibles, net") ~ "Intangible Assets Including Goodwill",

        standard_name %in% c("Finite-Lived Intangible Assets, Net", "Intangible and other assets", 
                             "Intangible assets, net","Intangibles and Other Assets, Net", 
                             "Intangibles and other assets, net", "Other intangible assets, net") ~ "Intangible Assets Excluding Goodwill",
        
        # Short-Term Debts
        standard_name %in% c("Convertible debt, current portion","Current portion of debt",
                             "Current portion of long-term debt","Long-term debt due within one year",
                             "Short-term debt") ~ "Short-Term debt",
        
        # Long-Term Debts
        standard_name %in% c("Convertible debt, net of current portion","Debt","Debt Net",
                             "Long Term Line of Credit and Long Term Debt excluding Current Maturities",
                             "Long-term debt", "Long-term Debt", "Long-Term Debt", "Long-term debt, net of current portion",
                             "Nonrecourse project debt", "Residual interests classified as debt", 
                             "Term loan, less debt issuance costs") ~ "Long-Term debt",
        
        # Capital Lease and Obligations
        standard_name %in% c("Capital lease obligation", "Capital Lease Obligations Current",
                             "Capital Lease Obligations Noncurrent") ~ "Capital Lease and Obligations",
        
        #TO CONTINUE WITH OTHER standard_name for ROC and EY
        
        TRUE ~ standard_name  # For other cases, keep the original standard_name
      )
    ) %>% select(harmonized_standard_name,everything())
  
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
  
  # Semi-join with filtereDF data frame to add marketCapitalization
  harmonized_filtered_data <- left_join(harmonized_filtered_data, filteredDF, by = "symbol") %>% 
    mutate(std_name =  harmonized_standard_name) %>% 
    select(symbol,std_name,value,unit,year,quarter, marketCapitalization,shareOutstanding, 
           name, finnhubIndustry, type,filedDate,country,exchange, ipo, financialStatement,
           -concept_aux, -most_frequent_label,-standard_name,-logo,-weburl,-phone, -shareClassFIGI)
  
  # Calculate Tangible Capital Employed by using the last quarter's Total Assets, Goodwill,
  # and Intangible Assets Excluding Goodwill
  filtered_data2 <- harmonized_filtered_data %>%
    group_by(symbol, std_name, financialStatement) %>%
    summarise(value = last(value)) %>%
    ungroup() %>%
    pivot_wider(names_from = std_name, values_from = value) %>%
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
