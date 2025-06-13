
library(broom)

# Response variables
response_vars <- c("median_secchi", "median_totnuf", "median_totpuf", "median_chla", "dWL")

# Model and extract summary stats
model_summary_table <- summary_medians %>%
  pivot_longer(cols = all_of(response_vars), names_to = "response", values_to = "value") %>%
  group_by(group, response) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(value ~ arb.precip, data = .x)),
    stats = map(model, glance)
  ) %>%
  unnest(stats) %>%
  select(group, response, r.squared, p.value)

# View the result
model_summary_table
