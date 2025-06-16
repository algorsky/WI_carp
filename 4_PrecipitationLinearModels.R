
library(broom)

# Response variables
response_vars <- c("mean_secchi", "mean_totnuf", "mean_totpuf", "mean_chla", "redblue", "mean_drsif")

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
  select(removal, response, r.squared, p.value)

# View the result
model_summary_table
