
library(broom)

# Response variables
response_vars <- c("mean_secchi", "mean_totnuf", "mean_totpuf", "mean_chla", "mean_tpm", "redblue", 
                   "plant_wt_spatial", "fil_algae_spatial", "mean_drsif")

# Model and extract summary stats
model_summary_table <- summary_means %>%
  pivot_longer(cols = all_of(response_vars), names_to = "response", values_to = "value") %>%
  group_by(removal, response) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(value ~ arb.precip, data = .x)),
    stats = map(model, glance)
  ) %>%
  unnest(stats) %>%
  dplyr::select(removal, response, r.squared, p.value) %>% 
  mutate(response = factor(response, levels = response_vars), NS = if_else(p.value <= 0.05, TRUE, FALSE)) %>% 
  arrange(response) 

# View the result
model_summary_table
