library(tidyverse)

# Join and fit models per 'removal' group for phosphorus
sig_models_TP <- tp %>% left_join(tn) |> 
  left_join(chloro_all) %>%
  group_by(removal) %>%
  do({
    model <- lm(chl_use ~ totpuf, data = .)
    tidy_model <- broom::tidy(model)
    slope_p <- tidy_model %>% filter(term == "totpuf") %>% pull(p.value)
    if (length(slope_p) == 0) slope_p <- NA  # edge case handling
    mutate(., sig_line = slope_p < 0.05, p = slope_p)
  }) %>%
  ungroup()

tp1 = tp |> left_join(chloro_all) |> 
  ggplot(aes(x = totpuf, y = chl_use)) +
  # Plot sig-only lines
  geom_smooth(
    data = filter(sig_models_TP, sig_line),
    aes(group = removal, linetype = removal),
    method = "lm",
    color = "black", size = 0.4
  ) +
  geom_point(aes(y = chl_use, x = totpuf, fill = removal),shape = 21, size = 1, stroke = 0.3) +
  scale_fill_manual(values = c( "white", "grey20"))+
  scale_linetype_manual(values = c(2,1)) +
  ylab(expression(paste("Chlorophyll a", " (µg ", L^-1,")")))+
  xlab(expression(paste("Total Phosphorus", " (µg ", L^-1,")")))+
  theme_bw(base_size = 9)

# Join and fit models per 'removal' group for nitrogen 
sig_models_TN <- tp %>% left_join(tn) |> 
  left_join(chloro_all) %>%
  group_by(removal) %>%
  do({
    model <- lm(chl_use ~ totnuf, data = .)
    tidy_model <- broom::tidy(model)
    slope_p <- tidy_model %>% filter(term == "totnuf") %>% pull(p.value)
    if (length(slope_p) == 0) slope_p <- NA  # edge case handling
    mutate(., sig_line = slope_p < 0.05, p = slope_p)
  }) %>%
  ungroup()

tn1 = tn |> left_join(chloro_all) |> 
  ggplot(aes(x = totnuf, y = chl_use)) +
  # Plot sig-only lines
  geom_smooth(
    data = filter(sig_models_TN, sig_line),
    aes(group = removal, linetype = removal),
    method = "lm",
    color = "black", size = 0.4
  ) +
  geom_point(aes(y = chl_use, x = totnuf, fill = removal),shape = 21, size = 1, stroke = 0.3) +
  scale_fill_manual(values = c( "white", "grey20"))+
  scale_linetype_manual(values = c(2,1)) +
  ylab(expression(paste("Chlorophyll a", " (µg ", L^-1,")")))+
  xlab(expression(paste("Total Nitrogen", " (µg ", L^-1,")")))+
  theme_bw(base_size = 9)

tp1 + tn1 +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") + plot_layout(guides='collect') &
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8), 
        legend.margin = margin(c(0,0,0,0),'cm')) 

ggsave("figures/Figure4.png", width = 6.5, height = 3, units = 'in', dpi = 500)
