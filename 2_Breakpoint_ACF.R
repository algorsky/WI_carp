# Breakpoint Analysis with Autocorrelation Adjustment
library(trend)
library(dplyr)
library(tibble)

# ---- Function to run Pettitt’s test and account for autocorrelation ----
run_pettitt_with_autocorr <- function(df, var_name, date_col = "sampledate") {
  df_clean <- df %>%
    arrange(.data[[date_col]]) %>%
    filter(!is.na(.data[[var_name]]))
  
  N <- nrow(df_clean)
  if (N < 2) {
    return(tibble(
      variable   = var_name,
      break_date = NA,
      break_date2 = NA,
      p_value    = NA,
      U_stat     = NA,
      rho1       = NA,
      N_eff      = NA,
      adj_p      = NA,
      adjusted_significant = NA
    ))
  }
  
  # --- 1. Compute lag-1 autocorrelation ---
  rho1 <- tryCatch(
    acf(df_clean[[var_name]], lag.max = 1, plot = FALSE)$acf[2],
    error = function(e) NA
  )
  
  if (is.na(rho1)) rho1 <- 0  # fallback if acf fails (e.g., all constant)
  
  # --- 2. Compute effective sample size ---
  # N_eff <- N * (1 - rho1) / (1 + rho1)
  # N_eff <- max(2, N_eff) # avoid division by 0 or tiny numbers
  
  N_eff = N / (1 + 2 * rho1)
  
  # --- 3. Run Pettitt’s test as usual ---
  test_result <- pettitt.test(df_clean[[var_name]])
  break_date <- df_clean[[date_col]][test_result$estimate]
  break_date2 <- df_clean[[date_col]][min(test_result$estimate + 1, N)]
  
  # --- 4. Adjusted interpretation ---
  adj_p <- min(1, test_result$p.value * (N / N_eff)) # scale p-value upward if autocorrelation present
  adj_sig <- ifelse(adj_p < 0.05 & N_eff > 20, TRUE, FALSE)
  
  # --- 5. Return results ---
  tibble(
    variable   = var_name,
    break_date = as.character(break_date),
    break_date2 = as.character(break_date2),
    p_value    = signif(test_result$p.value, 4),
    adj_p      = signif(adj_p, 4),
    U_stat     = test_result$statistic,
    rho1       = round(rho1, 3),
    N          = N,
    N_eff      = round(N_eff, 1),
    adjusted_significant = adj_sig
  )
}

# ---- Run Pettitt’s test on all datasets ----
pettitt_full_summary <- bind_rows(
  run_pettitt_with_autocorr(secchi, "secnview"),
  run_pettitt_with_autocorr(tn, "totnuf"),
  run_pettitt_with_autocorr(tp, "totpuf"),
  run_pettitt_with_autocorr(tpm, "tpm"),
  run_pettitt_with_autocorr(chloro_all, "chl_use"),
  run_pettitt_with_autocorr(ls7, "redblue")
)

# ---- Filter to only adjusted significant results ----
pettitt_significant <- pettitt_full_summary %>%
  filter(adjusted_significant == TRUE)

# ---- Print summary table with midpoint of breakpoints ----
print(
  pettitt_significant %>%
    mutate(
      break_date  = as.Date(break_date),
      break_date2 = as.Date(break_date2),
      midpoint = break_date + as.numeric(difftime(break_date2, break_date, units = "days")) / 2
    )
)

# ---- Run for annual time series ----
run_pettitt_with_autocorr(colonization, "colonization_m", date_col = "year4")
run_pettitt_with_autocorr(benthic_spatial_year, "plant_wt_spatial", date_col = "year4")
run_pettitt_with_autocorr(benthic_spatial_year, "fil_algae_spatial", date_col = "year4")
