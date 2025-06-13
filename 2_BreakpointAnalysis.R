library(dplyr)
library(trend)
library(tibble)

chloro_breakpoint<- chloro_all|> filter(year4 > 2004)

library(dplyr)
library(trend)
library(tibble)

# Function to run Pettitt's test and return p-value and include U* statistic
run_pettitt_with_U <- function(df, var_name, date_col = "sampledate") {
  df_clean <- df %>%
    arrange(.data[[date_col]]) %>%
    filter(!is.na(.data[[var_name]]))
  
  if (nrow(df_clean) < 2) {
    return(tibble(variable = var_name, break_date = NA, p_value = NA, U_stat = NA))
  }
  
  test_result <- pettitt.test(df_clean[[var_name]])
  break_date <- df_clean[[date_col]][test_result$estimate]
  
  tibble(
    variable   = var_name,
    break_date = as.character(break_date),
    p_value    = signif(test_result$p.value, 4),
    U_stat     = test_result$statistic
  )
}

# Run Pettitt's test on all four variables
pettitt_full_summary <- bind_rows(
  run_pettitt_with_U(secchi_all, "secnview"),
  run_pettitt_with_U(tn, "totnuf"),
  run_pettitt_with_U(tp, "totpuf"),
  run_pettitt_with_U(chloro_breakpoint, "chl_use"),
)

# Filter to only significant results
pettitt_significant <- pettitt_full_summary %>%
  filter(p_value < 0.05)

print(pettitt_significant)

